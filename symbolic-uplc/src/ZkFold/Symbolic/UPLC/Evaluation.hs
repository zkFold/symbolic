{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoStarIsType #-}

module ZkFold.Symbolic.UPLC.Evaluation (Sym, ExValue (..), MaybeValue (..), eval) where

import Control.Monad (return, (>>=))
import Data.Either (Either (..))
import Data.Foldable (toList)
import Data.Function (const, flip, ($), (.))
import Data.List (concatMap, map, null, (++))
import Data.Maybe (Maybe (..), fromJust, listToMaybe)
import Data.Ord ((<))
import Data.Text (unpack)
import Data.Traversable (traverse)
import Data.Typeable (cast)
import GHC.Generics (U1 (..), type (:*:) (..))
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.EllipticCurve.Class (Weierstrass (..))
import ZkFold.Algebra.Number
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq qualified as Symbolic
import ZkFold.Prelude (unsnoc, (!!))
import ZkFold.Symbolic.Algorithm.Hash.Blake2b (blake2b_224, blake2b_256)
import ZkFold.Symbolic.Algorithm.Hash.Keccak (keccakVar)
import ZkFold.Symbolic.Algorithm.Hash.SHA2 (sha2Var)
import ZkFold.Symbolic.Class (BaseField)
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..), all, bool)
import ZkFold.Symbolic.Data.ByteString (ByteString, dropN, reverseEndianness, truncate)
import ZkFold.Symbolic.Data.Class (SymbolicData (HasRep))
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.FFA (unsafeFromInt)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Int (Int (..), isNegative, isNotNegative, quot, rem)
import ZkFold.Symbolic.Data.List qualified as L
import ZkFold.Symbolic.Data.Maybe qualified as Symbolic
import ZkFold.Symbolic.Data.Ord qualified as Symbolic
import ZkFold.Symbolic.Data.UInt (UInt)
import ZkFold.Symbolic.Data.VarByteString
import Prelude (error, foldr, fromIntegral, toInteger)

import ZkFold.Symbolic.UPLC.Class
import ZkFold.Symbolic.UPLC.Constants
import ZkFold.Symbolic.UPLC.Data qualified as Data
import ZkFold.Symbolic.UPLC.Fun
import ZkFold.UPLC.BuiltinFunction
import ZkFold.UPLC.BuiltinType
import ZkFold.UPLC.Constant
import ZkFold.UPLC.Data
import ZkFold.UPLC.Term

------------------------------- MAIN ALGORITHM ---------------------------------

-- This part is not meant to be changed when extending the Converter
-- (except for bugfixing, of course).

-- | According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf),
-- evaluation of a UPLC term is a partial function.
--
-- In addition, successful evaluation of a smart contract compiled to UPLC
-- should yield a value of a builtin type.
--
-- We encode this with a Symbolic 'Maybe' of an arbitrary 'IsData'.
data MaybeValue c
  = forall t v. (IsData t v, HasRep v c) => MaybeValue (Symbolic.Maybe v c)

-- | Evaluation function.
--
-- Given enough arguments, we can evaluate a first-order
-- (that is, not taking functions as arguments)
-- UPLC term into a value of builtin type.
eval :: forall c. Sym c => Term -> [ExValue c] -> MaybeValue c
eval t args = case impl [] t $ map aValue args of
  Just v -> v
  Nothing -> MaybeValue (Symbolic.nothing :: Symbolic.Maybe U1 c)

-- | Type actually used inside Converter to represent evaluation result.
--
-- @Nothing@ is used to encode failure, just as @Just nothing@, but is used in
-- case when the expected type of a term is unknown.
--
-- @Just nothing@ is used to encode failure in cases when the expected type is
-- known, but the success/failure outcome is undecidable (that is, context
-- doesn't provide concrete values).
--
-- The case when both the success/failure outcome and type are unknown is
-- impossible.
type SomeValue c = Maybe (MaybeValue c)

-- | Type of terms/values stored in a term environment during evaluation.
--
-- The reason we store unevaluated terms in an environment is to evaluate
-- higher-rank programs correctly, e.g. @(\x -> ...) 'IfThenElse'@.
type Thunk c = Either Term (SomeValue c)

-- | With [De Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index) as variables,
-- an [environment](https://en.wikipedia.org/wiki/Typing_environment) is just a list of thunks.
type Env c = [Thunk c]

-- | Arguments to UPLC terms that we apply to obtain the result.
--
-- Note that we represent pattern-matching on constructors as an argument to
-- defer pattern-matching until we reach the actual constructor being matched.
data Arg c = ACase [Term] | AThunk (Thunk c)

-- | Actual evaluation function.
impl
  :: Sym c
  => Env c
  -- ^ Evaluation environment (thunks accessed by De Bruijn indices).
  -> Term
  -- ^ Term in question.
  -> [Arg c]
  -- ^ List of arguments to apply to the term.
  -> SomeValue c
  -- ^ Result of an evaluation.
impl env (TVariable i) args = applyVar env i args -- inspect environment
impl _ (TConstant c) [] = someValue (evalConstant c) -- embed constant inside context
impl _ (TConstant _) (_ : _) = Nothing -- constants are not functions!
impl env (TBuiltin f) args = applyBuiltin env f args -- inspect builtin
impl _ (TLam _) [] = Nothing -- not enough arguments supplied!
impl _ (TLam _) (ACase _ : _) = Nothing -- lambda cannot be pattern-matched!
impl env (TLam f) (AThunk t : args) = beta t env f args -- eval body
impl env (TApp f x) args = impl env f (aTerm x : args) -- prepend new arg for eval of a function
impl env (TDelay t) args = impl env t args -- we skip delays. Maybe wrong, but simpler
impl env (TForce t) args = impl env t args -- we skip forcings. Maybe wrong, but simpler.
impl env (TConstr t f) (ACase bs : args) = impl env (bs !! fromIntegral t) (map aTerm f ++ args) -- pattern-matching
impl env (TConstr t f) [] = traverse (\fi -> impl env fi []) f >>= constr t -- embed constructor
impl _ (TConstr _ _) (_ : _) = Nothing -- constructors are not functions!
impl env (TCase s bs) args = impl env s (ACase bs : args) -- defer pattern-matching
impl _ TError _ = Nothing -- errors are errors!

-- | Inspect the thunk pointed at by the variable.
applyVar :: Sym c => Env c -> DeBruijnIndex -> [Arg c] -> SomeValue c
applyVar ctx i args = case ctx !! i of
  Left t -> impl ctx t args -- evaluate the term
  Right v -> if null args then v else Nothing -- data are not functions!

-- | Prepares context and arguments to enter new scope, and evaluates the body.
-- Classic stuff when working with de Bruijn indices.
beta :: Sym c => Thunk c -> Env c -> Term -> [Arg c] -> SomeValue c
beta e env t args = impl (e : map shiftT env) t (map shiftA args)
 where
  shiftA (ACase ts) = ACase (map (`shift` 0) ts)
  shiftA (AThunk thunk) = AThunk (shiftT thunk)
  shiftT (Left term) = Left (shift term 0)
  shiftT (Right val) = Right val
  shift (TVariable i) b = TVariable (i + if i < b then 0 else 1)
  shift (TConstant c) _ = TConstant c
  shift (TBuiltin f) _ = TBuiltin f
  shift (TLam body) b = TLam $ shift body (b + 1)
  shift (TApp fun arg) b = TApp (shift fun b) (shift arg b)
  shift (TDelay term) b = TDelay (shift term b)
  shift (TForce term) b = TForce (shift term b)
  shift (TConstr tag fields) b = TConstr tag $ map (`shift` b) fields
  shift (TCase scr brs) b = TCase (shift scr b) $ map (`shift` b) brs
  shift TError _ = TError

-- | Inspect the builtin.
applyBuiltin :: Sym c => Env c -> BuiltinFunction s t -> [Arg c] -> SomeValue c
applyBuiltin ctx (BFMono f) args = applyMono true (evalMono f) [evalArg ctx a [] | a <- args]
applyBuiltin ctx (BFPoly f) args = applyPoly ctx f args

-- | Try to turn the argument into a value.
evalArg :: Sym c => Env c -> Arg c -> [Arg c] -> SomeValue c
evalArg _ (ACase _) _ = Nothing
evalArg ctx (AThunk (Left t)) args = impl ctx t args
evalArg _ (AThunk (Right v)) [] = v
evalArg _ (AThunk (Right _)) (_ : _) = Nothing

-- | Helper embedding function.
someValue :: Sym c => SymValue t c -> SomeValue c
someValue (SymValue v) = Just $ MaybeValue (Symbolic.just v)

-- | Helper embedding function.
aTerm :: Term -> Arg c
aTerm = AThunk . Left

-- | Helper embedding function.
aValue :: Sym c => ExValue c -> Arg c
aValue (ExValue v) = AThunk . Right . Just $ MaybeValue (Symbolic.just v)

-- | Apply the monomorphic function to its arguments, fully saturated.
--
-- Straightforward:
-- 1. given an argument, try to convert to the correct type;
-- 2. go further.
applyMono :: Sym c => Bool c -> Fun s t c -> [SomeValue c] -> SomeValue c
applyMono b (FSat x) [] =
  Just $ MaybeValue (bool Symbolic.nothing x b)
applyMono _ (FSat _) (_ : _) = Nothing
applyMono _ (FLam _) [] = Nothing
applyMono b (FLam f) (v : args) = do
  MaybeValue v' <- v
  mx <- cast v'
  applyMono (b && Symbolic.isJust mx) (f $ Symbolic.fromJust mx) args

--------------------------- BUILTINS INTERPRETATION ----------------------------

-- This part is meant to be changed when completing the Converter implementation.

-- | Apply polymorphic function to its arguments, fully saturated.
--
-- General algorithm:
-- * If this is a "destructuring" operation:
--     a. Evaluate a single operand as a builtin datatype;
--     b. Apply the destructuring function.
--
-- * If this is a "choice" operation:
--     a. Evaluate first operand as a builtin datatype;
--     b. Evaluate branches with the rest of the args supplied as their args;
--     c. Cast branches to the same type;
--     d. Apply the choice function.
--
-- Be careful with two layers of Maybe! Think about which errors do you wish to
-- propagate.
applyPoly :: forall c s t. Sym c => Env c -> BuiltinPolyFunction s t -> [Arg c] -> SomeValue c
applyPoly ctx IfThenElse (ct : tt : et : args) = do
  MaybeValue c0 <- evalArg ctx ct []
  withArms (evalArg ctx tt args) (evalArg ctx et args) $ \t e0 -> do
    c :: Symbolic.Maybe Bool c <- cast c0
    e <- cast e0
    return $ MaybeValue (Symbolic.maybe Symbolic.nothing (bool e t) c)
applyPoly ctx ChooseUnit (_ : t : args) = evalArg ctx t args
applyPoly ctx Trace (_ : t : args) = evalArg ctx t args
applyPoly ctx FstPair [arg] = do
  MaybeValue p <- evalArg ctx arg []
  (ExValue v, _) <- asPair (Symbolic.fromJust p)
  return $ MaybeValue (symMaybe p v)
applyPoly ctx SndPair [arg] = do
  MaybeValue p <- evalArg ctx arg []
  (_, ExValue v) <- asPair (Symbolic.fromJust p)
  return $ MaybeValue (symMaybe p v)
applyPoly ctx (BPFList ChooseList) (ct : tt : et : args) =
  let
    b0 = applyPoly ctx (BPFList NullList) [ct]
    bt = evalArg ctx (AThunk . Right $ b0) []
   in
    applyPoly ctx IfThenElse ((AThunk . Right $ bt) : tt : et : args)
applyPoly ctx (BPFList MkCons) [x0, xs0] = do
  MaybeValue x <- evalArg ctx x0 []
  let l = Symbolic.fromJust x
  MaybeValue xs <- evalArg ctx xs0 []
  let ls = Symbolic.fromJust xs
  ls' <- cast ls
  return $ MaybeValue (symMaybe2 x xs $ l L..: ls')
applyPoly ctx (BPFList HeadList) [xs0] = do
  MaybeValue p <- evalArg ctx xs0 []
  ExList v <- asList (Symbolic.fromJust p)
  return $ MaybeValue (symMaybe p (L.head v))
applyPoly ctx (BPFList TailList) [xs0] = do
  MaybeValue p <- evalArg ctx xs0 []
  ExList v <- asList (Symbolic.fromJust p)
  return $ MaybeValue (symMaybe p (L.tail v))
applyPoly ctx (BPFList NullList) [xs0] = do
  MaybeValue p <- evalArg ctx xs0 []
  ExList v <- asList (Symbolic.fromJust p)
  return $ MaybeValue (symMaybe p (L.null v))
applyPoly ctx ChooseData (dt : c0 : m0 : l0 : i0 : b0 : args) = do
  MaybeValue (cast -> Just (dv :: Symbolic.Maybe Data.Data c)) <- evalArg ctx dt []
  let brs0 = map (\x -> evalArg ctx x args) [c0, m0, l0, i0, b0]
  MaybeValue (_ :: Symbolic.Maybe d c) <- listToMaybe (concatMap toList brs0)
  [cv, mv, lv, iv, bv :: Symbolic.Maybe d c] <- traverse (>>= \(MaybeValue v) -> cast v) brs0
  return $
    MaybeValue $
      flip (Symbolic.maybe Symbolic.nothing) dv $
        flip Data.unfoldData \case
          Data.DConstrCell _ _ -> cv
          Data.DMapCell _ -> mv
          Data.DListCell _ -> lv
          Data.DIntCell _ -> iv
          Data.DBSCell _ -> bv
applyPoly _ _ _ = Nothing

-- | Correct error propagation for if-then-else
withArms
  :: Sym c
  => SomeValue c
  -> SomeValue c
  -> ( forall s t u v
        . (IsData s u, HasRep u c, IsData t v, HasRep v c)
       => Symbolic.Maybe u c -> Symbolic.Maybe v c -> Maybe r
     )
  -> Maybe r
withArms (Just (MaybeValue t)) (Just (MaybeValue e0)) f = f t e0
withArms (Just (MaybeValue @_ @_ @u t)) Nothing f = f t (Symbolic.nothing @u)
withArms Nothing (Just (MaybeValue @_ @_ @v e0)) f = f (Symbolic.nothing @v) e0
withArms Nothing Nothing _ = Nothing

-- | Helper function.
symMaybe :: (Sym c, IsData d t, HasRep t c) => Symbolic.Maybe u c -> t c -> Symbolic.Maybe t c
symMaybe b v = bool Symbolic.nothing (Symbolic.just v) (Symbolic.isJust b)

symMaybe2 :: (Sym c, IsData d t, HasRep t c) => Symbolic.Maybe u c -> Symbolic.Maybe w c -> t c -> Symbolic.Maybe t c
symMaybe2 b1 b2 v = bool Symbolic.nothing (Symbolic.just v) (Symbolic.isJust b1 && Symbolic.isJust b2)

-- | Some Symbolic value of a definite UPLC builtin type.
data SymValue t c = forall v. (IsData t v, HasRep v c) => SymValue (v c)

-- | Given a UPLC constant, evaluate it as a corresponding Symbolic value.
-- Types would not let you go (terribly) wrong!
evalConstant :: forall c t. Sym c => Constant t -> SymValue t c
evalConstant (CBool b) = SymValue (if b then true else false)
evalConstant (CInteger i) = withNumberOfRegisters @IntLength @Auto @(BaseField c) $ SymValue (fromConstant i)
evalConstant (CByteString b) = SymValue (fromConstant b)
evalConstant (CString s) = SymValue (fromString $ unpack s)
evalConstant (CUnit ()) = SymValue U1
evalConstant (CData d) = SymValue (fromConstant d)
evalConstant (CList []) = error "TODO: handle empty list constants"
evalConstant (CList lst) =
  let (xs, x) = fromJust (unsnoc lst)
   in foldr (consList . evalConstant) (makeSingleton (evalConstant x)) xs
evalConstant (CPair p q) = pair (evalConstant p) (evalConstant q)
evalConstant (CG1 p) = SymValue (fromConstant p)
evalConstant (CG2 _) = error "TODO: add proper G2 support"

-- | Useful helper function.
pair :: Sym c => SymValue t c -> SymValue u c -> SymValue (BTPair t u) c
pair (SymValue p) (SymValue q) = SymValue (p :*: q)

makeSingleton :: Sym c => SymValue u c -> SymValue (BTList u) c
makeSingleton (SymValue xs) = SymValue $ L.singleton xs

consList :: forall c u. Sym c => SymValue u c -> SymValue (BTList u) c -> SymValue (BTList u) c
consList (SymValue (x :: v)) (SymValue (xs :: vs c)) =
  SymValue @_ @_ @vs . fromJust . cast $ x L..: fromJust (cast xs)

-- | Given a tag and fields, evaluate them as an instance of UPLC Data type.
constr :: forall c. Sym c => ConstructorTag -> [MaybeValue c] -> SomeValue c
constr (fromConstant . toInteger -> cTag) fields0 = do
  let isJust = all (\(MaybeValue v) -> Symbolic.isJust v) fields0
  fields1 :: [Data.Data c] <- traverse (\(MaybeValue v) -> asData $ Symbolic.fromJust v) fields0
  return $
    MaybeValue $
      Symbolic.guard isJust $
        Data.foldData $
          Data.DConstrCell cTag (fromConstant fields1)

-- | Given a monomorphic UPLC builtin, evaluate it
-- as a corresponding Symbolic function.
--
-- Types will not let you go (terribly) wrong!
--
-- Note that you can use 'FromConstant' instances defined above
-- to get rid of the 'FSat'/'FLam' boilerplate.
evalMono :: forall c s t. Sym c => BuiltinMonoFunction s t -> Fun s t c
evalMono (BMFInteger fun) = case fun of
  AddInteger -> fromConstant \v w -> Symbolic.just @c (v + w)
  SubtractInteger -> fromConstant \v w -> Symbolic.just @c (v - w)
  MultiplyInteger -> fromConstant \v w -> Symbolic.just @c (v * w)
  DivideInteger -> fromConstant \v w -> Symbolic.just @c (div v w)
  ModInteger -> fromConstant \v w -> Symbolic.just @c (mod v w)
  QuotientInteger -> fromConstant \v w -> Symbolic.just @c (quot v w)
  RemainderInteger -> fromConstant \v w -> Symbolic.just @c (rem v w)
  EqualsInteger -> FLam \v -> FLam \w -> FSat $ Symbolic.just @c (v Symbolic.== w)
  LessThanInteger -> FLam \v -> FLam \w -> FSat $ Symbolic.just @c (v Symbolic.< w)
  LessThanEqualsInteger -> FLam \v -> FLam \w -> FSat $ Symbolic.just @c (v Symbolic.<= w)
  IntegerToByteString -> FLam \b -> FLam \w -> FLam \(Int i) ->
    FSat $
      Symbolic.guard
        (isNotNegative w)
        VarByteString
          { bsLength =
              from @(UInt (NumberOfBits (BaseField c)) IntRegSize c) $
                resize (uint w)
          , bsBuffer =
              resize $
                let res = from i :: ByteString IntLength c
                 in ifThenElse b res (reverseEndianness @8 res)
          }
  ByteStringToInteger -> FLam \b -> FLam \(VarByteString _ bs) ->
    FSat $
      Symbolic.just @c . Int . from @(ByteString IntLength c) . resize $
        ifThenElse b bs (reverseEndianness @8 bs)
evalMono (BMFByteString fun) = case fun of
  AppendByteString -> fromConstant \v w -> Symbolic.just @c (v `app` w)
  ConsByteString -> FLam \v -> FLam \w ->
    let bu :: ByteString 8 c = dropN @8 @IntLength (from $ uint v)
     in FSat $ Symbolic.just @c $ dropZeros @BSLength (append w $ fromByteString bu)
  SliceByteString -> FLam \(Int f) -> FLam \(Int t) -> FLam \(VarByteString l b) ->
    let f' = from (resize f :: UInt (NumberOfBits (BaseField c)) Auto c)
        t' = from (resize t :: UInt (NumberOfBits (BaseField c)) Auto c)
        (fr, to) = (Symbolic.max f' zero, Symbolic.min (f' + t' - one) (l - one))
        fe8 = fromConstant (8 :: Natural) :: FieldElement c
        newB =
          shiftR
            (shiftL b (fromConstant (value @BSLength) - l + fr * fe8))
            (fromConstant (value @BSLength) - to * fe8)
     in FSat $ Symbolic.just @c (VarByteString to newB)
  LengthOfByteString -> FLam \(VarByteString l _) ->
    let l' = resize (from l :: UInt (NumberOfBits (BaseField c)) Auto c)
     in FSat $ Symbolic.just @c $ div (Int l') (fromConstant (8 :: Natural))
  IndexByteString -> FLam \(VarByteString l b) -> FLam \i@(Int t) ->
    let indexIn = (isNotNegative i && t Symbolic.< fromConstant (value @BSLength))
        fr = from (resize t :: UInt (NumberOfBits (BaseField c)) Auto c)
        fe8 = fromConstant (8 :: Natural) :: FieldElement c
        newB =
          shiftR
            (shiftL b (fromConstant (value @BSLength) - l + fr * fe8))
            (fromConstant (value @BSLength) - fe8)
     in FSat $ bool Symbolic.nothing (Symbolic.just @c (Int . from $ truncate @_ @IntLength newB)) indexIn
  EqualsByteString -> fromConstant \v w -> Symbolic.just @c $ bsBuffer v Symbolic.== bsBuffer w
  LessThanByteString -> FLam \v -> FLam \w ->
    FSat $ Symbolic.just @c $ (from (bsBuffer v) :: UInt BSLength Auto c) Symbolic.< from (bsBuffer w)
  LessThanEqualsByteString -> FLam \v -> FLam \w ->
    FSat $ Symbolic.just @c $ (from (bsBuffer v) :: UInt BSLength Auto c) Symbolic.<= from (bsBuffer w)
evalMono (BMFString fun) = case fun of
  AppendString -> fromConstant \v w -> Symbolic.just @c (v `app` w)
  EqualsString -> fromConstant \v w -> Symbolic.just @c $ bsBuffer v Symbolic.== bsBuffer w
  EncodeUtf8 -> fromConstant (Symbolic.just @c . dropZeros @BSLength)
  DecodeUtf8 -> fromConstant \(VarByteString l b) -> Symbolic.just @c $ VarByteString l (resize b)
evalMono (BMFAlgorithm fun) = case fun of
  SHA2_256 -> FLam \v ->
    FSat $
      Symbolic.just @c
        VarByteString
          { bsLength = fromConstant (256 :: Natural)
          , bsBuffer = resize (sha2Var @"SHA256" v)
          }
  SHA3_256 -> FLam \v ->
    FSat $
      Symbolic.just @c
        VarByteString
          { bsLength = fromConstant (256 :: Natural)
          , bsBuffer = resize (keccakVar @"SHA3-256" v)
          }
  Blake2b_224 -> FLam \v ->
    FSat $
      Symbolic.just @c
        VarByteString
          { bsLength = fromConstant (224 :: Natural)
          , bsBuffer = resize $ blake2b_224 (bsBuffer v)
          }
  Blake2b_256 -> FLam \v ->
    FSat $
      Symbolic.just @c
        VarByteString
          { bsLength = fromConstant (256 :: Natural)
          , bsBuffer = resize $ blake2b_256 (bsBuffer v)
          }
  VerifyEd25519Signature -> FLam \vk -> FLam \m -> FLam \s ->
    FSat $
      Symbolic.guard
        ( bsLength vk
            Symbolic.== fromConstant (32 * 8 :: Natural)
            && bsLength s
            Symbolic.== fromConstant (64 * 8 :: Natural)
        )
        (ecdsaVerifyVar (resize $ bsBuffer vk) m (resize $ bsBuffer s))
  VerifyEcdsaSecp256k1Signature -> error "TODO: verify ECDSA secp256k1 signature"
  VerifySchnorrSecp256k1Signature -> error "TODO: verify Schnorr secp256k1 signature"
  Keccak_256 -> FLam \v ->
    FSat $
      Symbolic.just @c
        VarByteString
          { bsLength = fromConstant (256 :: Natural)
          , bsBuffer = resize $ keccakVar @"Keccak256" v
          }
  Ripemd_160 -> error "TODO: RIPEMD-160"
evalMono (BMFData fun) = case fun of
  ConstrData -> FLam \(resize . uint -> t) ->
    FLam $
      FSat . Symbolic.just @c . Data.foldData . Data.DConstrCell t
  MapData -> fromConstant (Symbolic.just @c . Data.foldData . Data.DMapCell)
  ListData -> fromConstant (Symbolic.just @c . Data.foldData . Data.DListCell)
  IData -> fromConstant (Symbolic.just @c . Data.foldData . Data.DIntCell)
  BData -> fromConstant (Symbolic.just @c . Data.foldData . Data.DBSCell)
  UnConstrData ->
    fromConstant
      ( `Data.unfoldData`
          \case
            Data.DConstrCell (Int . resize -> t) f -> Symbolic.just @c (t :*: f)
            _ -> Symbolic.nothing
      )
  UnMapData ->
    fromConstant
      ( `Data.unfoldData`
          \case
            Data.DMapCell es -> Symbolic.just @c es
            _ -> Symbolic.nothing
      )
  UnListData ->
    fromConstant
      ( `Data.unfoldData`
          \case
            Data.DListCell xs -> Symbolic.just @c xs
            _ -> Symbolic.nothing
      )
  UnIData ->
    fromConstant
      ( `Data.unfoldData`
          \case
            Data.DIntCell int -> Symbolic.just @c int
            _ -> Symbolic.nothing
      )
  UnBData ->
    fromConstant
      ( `Data.unfoldData`
          \case
            Data.DBSCell bs -> Symbolic.just @c bs
            _ -> Symbolic.nothing
      )
  EqualsData -> FLam \d -> FLam \e -> FSat $ Symbolic.just @c (d Symbolic.== e)
  MkPairData -> fromConstant \d e -> Symbolic.just @c (d :*: e)
  MkNilData -> FLam $ const $ FSat $ Symbolic.just @c L.emptyList
  MkNilPairData -> FLam $ const $ FSat $ Symbolic.just @c L.emptyList
  SerializeData -> fromConstant (Symbolic.just @c . Data.serialiseData)
evalMono (BMFCurve fun) = case fun of
  BLS_G1 fn -> case fn of
    Bls12_381_G1_add -> FLam \p -> FLam \q ->
      FSat $ Symbolic.just @c (p + q)
    Bls12_381_G1_neg -> fromConstant (Symbolic.just @c . negate)
    Bls12_381_G1_scalarMul -> fromConstant \i p ->
      Symbolic.just @c (unsafeFromInt @c @BLS12_381_Scalar i `scale` p)
    Bls12_381_G1_equal -> FLam \p -> FLam \q -> FSat $ Symbolic.just @c (p Symbolic.== q)
    Bls12_381_G1_hashToGroup -> error "TODO: hash to G1"
    Bls12_381_G1_compress -> error "TODO: compress G1"
    Bls12_381_G1_uncompress -> error "TODO: uncompress G1"
  BLS_G2 fn -> case fn of
    Bls12_381_G2_add -> fromConstant \p q -> Symbolic.just @c (p + q)
    Bls12_381_G2_neg -> fromConstant (Symbolic.just @c . negate)
    Bls12_381_G2_scalarMul -> fromConstant \i p ->
      Symbolic.just @c (unsafeFromInt @c @BLS12_381_Scalar i `scale` p)
    Bls12_381_G2_equal -> FLam \p -> FLam \q ->
      FSat $ Symbolic.just (p Symbolic.== q)
    Bls12_381_G2_hashToGroup -> error "TODO: hash to G2"
    Bls12_381_G2_compress -> error "TODO: compress G2"
    Bls12_381_G2_uncompress -> error "TODO: uncompress G2"
  Bls12_381_millerLoop -> error "TODO: miller loop"
  Bls12_381_mulMlResult -> fromConstant \r s -> Symbolic.just @c (r + s)
  Bls12_381_finalVerify -> error "TODO: final verify"
evalMono (BMFBitwise fun) = case fun of
  AndByteString -> FLam \ext -> FLam \a -> FLam \b ->
    FSat $
      Symbolic.just @c $
        ifThenElse
          ext
          ( let n = bsLength a `Symbolic.max` bsLength b
             in VarByteString
                  { bsLength = n
                  , bsBuffer =
                      bsBuffer (a `app` ones (n - bsLength a))
                        && bsBuffer (b `app` ones (n - bsLength b))
                  }
          )
          VarByteString
            { bsLength = bsLength a `Symbolic.min` bsLength b
            , bsBuffer = bsBuffer a && bsBuffer b
            }
  OrByteString -> FLam \ext -> FLam \a -> FLam \b ->
    FSat $
      Symbolic.just @c $
        wipeUnassigned
          VarByteString
            { bsLength =
                ifThenElse
                  ext
                  (bsLength a `Symbolic.max` bsLength b)
                  (bsLength a `Symbolic.min` bsLength b)
            , bsBuffer = bsBuffer a || bsBuffer b
            }
  XorByteString -> FLam \ext -> FLam \a -> FLam \b ->
    FSat $
      Symbolic.just @c $
        wipeUnassigned
          VarByteString
            { bsLength =
                ifThenElse
                  ext
                  (bsLength a `Symbolic.max` bsLength b)
                  (bsLength a `Symbolic.min` bsLength b)
            , bsBuffer = bsBuffer a `xor` bsBuffer b
            }
  ComplementByteString -> fromConstant \bs ->
    Symbolic.just @c $
      wipeUnassigned bs {bsBuffer = not (bsBuffer bs)}
  ShiftByteString -> FLam \_bs -> FLam \i ->
    FSat $
      Symbolic.just @c $
        ifThenElse (isNegative i) (error "TODO: shiftL") (error "TODO: shiftR")
  RotateByteString -> FLam \_bs -> FLam \i ->
    FSat $
      Symbolic.just @c $
        ifThenElse (isNegative i) (error "TODO: rotL") (error "TODO: rotR")
  CountSetBits -> error "TODO: countBits"
  FindFirstSetBit -> error "TODO: findSet"
  ReadBit -> error "TODO: readBit"
  WriteBits -> error "TODO: writeBits"
  ReplicateByte -> error "TODO: replicateByte"

app
  :: (Sym c, KnownNat n, KnownNat (n + n), KnownNat ((n + n) - n), n <= n + n)
  => VarByteString n c -> VarByteString n c -> VarByteString n c
app l r = dropZeros (l `append` r)

ones :: Sym c => FieldElement c -> VarByteString BSLength c
ones n = wipeUnassigned (VarByteString n true)

ecdsaVerifyVar
  :: Sym c
  => ByteString (32 * 8) c
  -> VarByteString BSLength c
  -> ByteString (64 * 8) c
  -> Bool c
ecdsaVerifyVar = error "TODO: ECDSA Ed25519 verify"
