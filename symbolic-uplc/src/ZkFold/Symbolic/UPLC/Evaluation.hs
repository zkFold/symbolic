{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module ZkFold.Symbolic.UPLC.Evaluation (Sym, ExValue (..), MaybeValue (..), eval) where

import Control.Monad (return)
import Data.Either (Either (..))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List (map, null, (++))
import Data.Maybe (Maybe (..), fromJust)
import Data.Ord ((<))
import Data.Proxy (Proxy (..))
import Data.Text (unpack)
import Data.Traversable (traverse)
import Data.Typeable (cast)
import ZkFold.Algebra.Class (
  AdditiveMonoid (zero),
  FromConstant (..),
  MultiplicativeMonoid (..),
  NumberOfBits,
  (*),
  (+),
  (-),
 )
import ZkFold.Algebra.Number (Natural, value)
import ZkFold.Prelude (unsnoc, (!!))
import ZkFold.Symbolic.Algorithm.Hash.SHA2 (sha2Var)
import ZkFold.Symbolic.Class (BaseField)
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Data.ByteString (ByteString, dropN, truncate)
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.Conditional (bool)
import ZkFold.Symbolic.Data.Eq qualified as Symbolic
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Int
import ZkFold.Symbolic.Data.List qualified as L
import ZkFold.Symbolic.Data.Maybe qualified as Symbolic
import ZkFold.Symbolic.Data.Ord qualified as Symbolic
import ZkFold.Symbolic.Data.UInt (OrdWord, UInt)
import ZkFold.Symbolic.Data.VarByteString
import ZkFold.Symbolic.UPLC.Class
import ZkFold.Symbolic.UPLC.Fun
import ZkFold.UPLC.BuiltinFunction
import ZkFold.UPLC.BuiltinType
import ZkFold.UPLC.Term
import Prelude (error, foldr, fromIntegral)

------------------------------- MAIN ALGORITHM ---------------------------------

-- This part is not meant to be changed when extending the Converter
-- (except for bugfixing, of course).

{- | According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf),
evaluation of a UPLC term is a partial function.

In addition, successful evaluation of a smart contract compiled to UPLC
should yield a value of a builtin type.

We encode this with a Symbolic 'Maybe' of an arbitrary 'IsData'.
-}
data MaybeValue c = forall t v. IsData t v c => MaybeValue (Symbolic.Maybe c v)

{- | Evaluation function.

Given enough arguments, we can evaluate a first-order
(that is, not taking functions as arguments)
UPLC term into a value of builtin type.
-}
eval :: forall c. Sym c => Term -> [ExValue c] -> MaybeValue c
eval t args = case impl [] t $ map aValue args of
  Just v -> v
  Nothing -> MaybeValue (Symbolic.nothing :: Symbolic.Maybe c (Proxy c))

{- | Type actually used inside Converter to represent evaluation result.

@Nothing@ is used to encode failure, just as @Just nothing@, but is used in
case when the expected type of a term is unknown.

@Just nothing@ is used to encode failure in cases when the expected type is
known, but the success/failure outcome is undecidable (that is, context
doesn't provide concrete values).

The case when both the success/failure outcome and type are unknown is
impossible.
-}
type SomeValue c = Maybe (MaybeValue c)

{- | Type of terms/values stored in a term environment during evaluation.

The reason we store unevaluated terms in an environment is to evaluate
higher-rank programs correctly, e.g. @(\x -> ...) 'IfThenElse'@.
-}
type Thunk c = Either Term (SomeValue c)

{- | With [De Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index) as variables,
an [environment](https://en.wikipedia.org/wiki/Typing_environment) is just a list of thunks.
-}
type Env c = [Thunk c]

{- | Arguments to UPLC terms that we apply to obtain the result.

Note that we represent pattern-matching on constructors as an argument to
defer pattern-matching until we reach the actual constructor being matched.
-}
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
impl env (TConstr t f) [] = constr t <$> traverse (\fi -> impl env fi []) f -- embed constructor
impl _ (TConstr _ _) (_ : _) = Nothing -- constructors are not functions!
impl env (TCase s bs) args = impl env s (ACase bs : args) -- defer pattern-matching
impl _ TError _ = Nothing -- errors are errors!

-- | Inspect the thunk pointed at by the variable.
applyVar :: Sym c => Env c -> DeBruijnIndex -> [Arg c] -> SomeValue c
applyVar ctx i args = case ctx !! i of
  Left t -> impl ctx t args -- evaluate the term
  Right v -> if null args then v else Nothing -- data are not functions!

{- | Prepares context and arguments to enter new scope, and evaluates the body.
Classic stuff when working with de Bruijn indices.
-}
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

{- | Apply the monomorphic function to its arguments, fully saturated.

Straightforward:
1. given an argument, try to convert to the correct type;
2. go further.
-}
applyMono :: Sym c => Bool c -> Fun s t c -> [SomeValue c] -> SomeValue c
applyMono b (FSat x) [] = Just $ MaybeValue (bool Symbolic.nothing x b)
applyMono _ (FSat _) (_ : _) = Nothing
applyMono _ (FLam _) [] = Nothing
applyMono b (FLam f) (v : args) = do
  MaybeValue v' <- v
  mx <- cast v'
  applyMono (b && Symbolic.isJust mx) (f $ Symbolic.fromJust mx) args

--------------------------- BUILTINS INTERPRETATION ----------------------------

-- This part is meant to be changed when completing the Converter implementation.

{- | Apply polymorphic function to its arguments, fully saturated.

General algorithm:
* If this is a "destructuring" operation:
    a. Evaluate a single operand as a builtin datatype;
    b. Apply the destructuring function.

* If this is a "choice" operation:
    a. Evaluate first operand as a builtin datatype;
    b. Evaluate branches with the rest of the args supplied as their args;
    c. Cast branches to the same type;
    d. Apply the choice function.

Be careful with two layers of Maybe! Think about which errors do you wish to
propagate.
-}
applyPoly :: forall c s t. Sym c => Env c -> BuiltinPolyFunction s t -> [Arg c] -> SomeValue c
applyPoly ctx IfThenElse (ct : tt : et : args) = do
  MaybeValue c0 <- evalArg ctx ct []
  withArms (evalArg ctx tt args) (evalArg ctx et args) $ \t e0 -> do
    c :: Symbolic.Maybe c (Bool c) <- cast c0
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
applyPoly _ ChooseData _ = error "FIXME: UPLC Data support"
applyPoly _ _ _ = Nothing

-- | Correct error propagation for if-then-else
withArms
  :: Sym c
  => SomeValue c
  -> SomeValue c
  -> ( forall s t u v
        . (IsData s u c, IsData t v c)
       => Symbolic.Maybe c u -> Symbolic.Maybe c v -> Maybe r
     )
  -> Maybe r
withArms (Just (MaybeValue t)) (Just (MaybeValue e0)) f = f t e0
withArms (Just (MaybeValue @_ @_ @u t)) Nothing f = f t (Symbolic.nothing @u)
withArms Nothing (Just (MaybeValue @_ @_ @v e0)) f = f (Symbolic.nothing @v) e0
withArms Nothing Nothing _ = Nothing

-- | Helper function.
symMaybe :: (Sym c, IsData d t c) => Symbolic.Maybe c u -> t -> Symbolic.Maybe c t
symMaybe b v = bool Symbolic.nothing (Symbolic.just v) (Symbolic.isJust b)

symMaybe2 :: (Sym c, IsData d t c) => Symbolic.Maybe c u -> Symbolic.Maybe c w -> t -> Symbolic.Maybe c t
symMaybe2 b1 b2 v = bool Symbolic.nothing (Symbolic.just v) (Symbolic.isJust b1 && Symbolic.isJust b2)

-- | Some Symbolic value of a definite UPLC builtin type.
data SymValue t c = forall v. IsData t v c => SymValue v

{- | Given a UPLC constant, evaluate it as a corresponding Symbolic value.
Types would not let you go (terribly) wrong!
-}
evalConstant :: forall c t. Sym c => Constant t -> SymValue t c
evalConstant (CBool b) = SymValue (if b then true else false)
evalConstant (CInteger i) = withNumberOfRegisters @IntLength @Auto @(BaseField c) $ SymValue (fromConstant i)
evalConstant (CByteString b) = SymValue (fromConstant b)
evalConstant (CString s) = SymValue (fromString $ unpack s)
evalConstant (CUnit ()) = SymValue Proxy
evalConstant (CData _) = error "FIXME: UPLC Data support"
evalConstant (CList []) = error "FIXME: UPLC List support"
evalConstant (CList lst) =
  let (xs, x) = fromJust (unsnoc lst)
   in foldr (\l r -> consList l r) (makeSingleton (evalConstant x)) (map evalConstant xs)
evalConstant (CPair p q) = pair (evalConstant p) (evalConstant q)
evalConstant (CG1 _) = error "FIXME: UPLC BLS support"
evalConstant (CG2 _) = error "FIXME: UPLC BLS support"

-- | Useful helper function.
pair :: Sym c => SymValue t c -> SymValue u c -> SymValue (BTPair t u) c
pair (SymValue p) (SymValue q) = SymValue (p, q)

makeSingleton :: Sym c => SymValue u c -> SymValue (BTList u) c
makeSingleton (SymValue xs) = SymValue $ L.singleton xs

consList :: forall c u. Sym c => SymValue u c -> SymValue (BTList u) c -> SymValue (BTList u) c
consList (SymValue (x :: v)) (SymValue (xs :: vs)) = SymValue @_ @_ @vs (fromJust . cast $ x L..: (fromJust $ cast xs))

-- | Given a tag and fields, evaluate them as an instance of UPLC Data type.
constr :: Sym c => ConstructorTag -> [MaybeValue c] -> MaybeValue c
constr _ _ = error "FIXME: UPLC Data support"

{- | Given a monomorphic UPLC builtin, evaluate it
as a corresponding Symbolic function.

Types will not let you go (terribly) wrong!

Note that you can use 'FromConstant' instances defined above
to get rid of the 'FSat'/'FLam' boilerplate.
-}
evalMono :: forall c s t. Sym c => BuiltinMonoFunction s t -> Fun s t c
evalMono (BMFInteger AddInteger) = addIntegerFun
evalMono (BMFInteger SubtractInteger) = subtractIntegerFun
evalMono (BMFInteger MultiplyInteger) = multiplyIntegerFun
evalMono (BMFInteger DivideInteger) = divideIntegerFun
evalMono (BMFInteger ModInteger) = modIntegerFun
evalMono (BMFInteger QuotientInteger) = quotientIntegerFun
evalMono (BMFInteger RemainderInteger) = remainderIntegerFun
evalMono (BMFInteger EqualsInteger) = equalsIntegerFun
evalMono (BMFInteger LessThanInteger) = lessThanIntegerFun
evalMono (BMFInteger LessThanEqualsInteger) = lessThanEqualsIntegerFun
evalMono (BMFInteger _) = error "FIXME: UPLC Integer support"
evalMono (BMFByteString AppendByteString) = appendByteStringFun
evalMono (BMFByteString ConsByteString) = consByteStringFun
evalMono (BMFByteString SliceByteString) = sliceByteStringFun
evalMono (BMFByteString LengthOfByteString) = lengthOfByteStringFun
evalMono (BMFByteString IndexByteString) = indexByteStringFun
evalMono (BMFByteString EqualsByteString) = equalsByteStringFun
evalMono (BMFByteString LessThanByteString) = lessThanByteStringFun
evalMono (BMFByteString LessThanEqualsByteString) = lessThanEqualsByteStringFun
evalMono (BMFString AppendString) = appendStringFun
evalMono (BMFString EqualsString) = equalsStringFun
evalMono (BMFString EncodeUtf8) = encodeUtf8Fun
evalMono (BMFString DecodeUtf8) = decodeUtf8Fun
evalMono (BMFAlgorithm SHA2_256) = sha2_256Fun
evalMono (BMFAlgorithm _) = error "FIXME: UPLC Algorithms support"
evalMono (BMFData _) = error "FIXME: UPLC Data support"
evalMono (BMFCurve _) = error "FIXME: UPLC Curve support"
evalMono (BMFBitwise _) = error "FIXME: UPLC ByteString support"

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

addIntegerFun :: forall c. Sym c => Fun [BTInteger, BTInteger] BTInteger c
addIntegerFun =
  withNumberOfRegisters @IntLength @Auto @(BaseField c) $
    fromConstant (\v w -> Symbolic.just @c (v + w))

subtractIntegerFun :: forall c. Sym c => Fun [BTInteger, BTInteger] BTInteger c
subtractIntegerFun =
  withNumberOfRegisters @IntLength @Auto @(BaseField c) $
    fromConstant (\v w -> Symbolic.just @c (v - w))

multiplyIntegerFun :: forall c. Sym c => Fun [BTInteger, BTInteger] BTInteger c
multiplyIntegerFun =
  withNumberOfRegisters @IntLength @Auto @(BaseField c) $
    fromConstant (\v w -> Symbolic.just @c (v * w))

divideIntegerFun :: forall c. Sym c => Fun [BTInteger, BTInteger] BTInteger c
divideIntegerFun =
  withNumberOfRegisters @IntLength @Auto @(BaseField c) $
    fromConstant (\v w -> Symbolic.just @c (div v w))

modIntegerFun :: forall c. Sym c => Fun [BTInteger, BTInteger] BTInteger c
modIntegerFun =
  withNumberOfRegisters @IntLength @Auto @(BaseField c) $
    fromConstant (\v w -> Symbolic.just @c (mod v w))

quotientIntegerFun :: forall c. Sym c => Fun [BTInteger, BTInteger] BTInteger c
quotientIntegerFun =
  withNumberOfRegisters @IntLength @Auto @(BaseField c) $
    fromConstant (\v w -> Symbolic.just @c (quot v w))

remainderIntegerFun :: forall c. Sym c => Fun [BTInteger, BTInteger] BTInteger c
remainderIntegerFun =
  withNumberOfRegisters @IntLength @Auto @(BaseField c) $
    fromConstant (\v w -> Symbolic.just @c (rem v w))

equalsIntegerFun :: forall c. Sym c => Fun [BTInteger, BTInteger] BTBool c
equalsIntegerFun =
  withNumberOfRegisters @IntLength @Auto @(BaseField c) $
    fromConstant (\v w -> Symbolic.just @c (v Symbolic.== w))

lessThanIntegerFun :: forall c. Sym c => Fun [BTInteger, BTInteger] BTBool c
lessThanIntegerFun =
  withNumberOfRegisters @IntLength @Auto @(BaseField c) $
    fromConstant (\v w -> Symbolic.just @c (v Symbolic.< w))

lessThanEqualsIntegerFun :: forall c. Sym c => Fun [BTInteger, BTInteger] BTBool c
lessThanEqualsIntegerFun =
  withNumberOfRegisters @IntLength @Auto @(BaseField c) $
    fromConstant (\v w -> Symbolic.just @c (v Symbolic.<= w))

--------------------------------------------------------------------------------

appendByteStringFun :: forall c. Sym c => Fun [BTByteString, BTByteString] BTByteString c
appendByteStringFun = fromConstant (\v w -> Symbolic.just @c $ dropZeros @BSLength (append v w))

consByteStringFun :: forall c. Sym c => Fun [BTInteger, BTByteString] BTByteString c
consByteStringFun =
  withNumberOfRegisters @IntLength @Auto @(BaseField c) $
    fromConstant
      ( \v w ->
          let bu :: ByteString 8 c = dropN @8 @IntLength (from $ uint v)
           in Symbolic.just @c $ dropZeros @BSLength (append w $ fromByteString bu)
      )

sliceByteStringFun :: forall c. Sym c => Fun [BTInteger, BTInteger, BTByteString] BTByteString c
sliceByteStringFun =
  withNumberOfRegisters @IntLength @Auto @(BaseField c) $
    fromConstant
      ( \(Int f :: Int IntLength Auto c) (Int t :: Int IntLength Auto c) (VarByteString l b) ->
          let f' = from (resize f :: UInt (NumberOfBits (BaseField c)) Auto c)
              t' = from (resize t :: UInt (NumberOfBits (BaseField c)) Auto c)
              (fr, to) = (Symbolic.max f' zero, Symbolic.min (f' + t' - one) (l - one))
              fe8 = fromConstant (8 :: Natural) :: FieldElement c
              newB = shiftR (shiftL b (fromConstant (value @BSLength) - l + fr * fe8)) (fromConstant (value @BSLength) - to * fe8)
           in fromConstant $ Symbolic.just @c (VarByteString to newB) :: (Fun '[] BTByteString c)
      )

lengthOfByteStringFun :: forall c. Sym c => Fun '[BTByteString] BTInteger c
lengthOfByteStringFun =
  withNumberOfRegisters @IntLength @Auto @(BaseField c) $
    fromConstant
      ( \(VarByteString l _) ->
          let l' = resize (from l :: UInt (NumberOfBits (BaseField c)) Auto c)
           in Symbolic.just @c $ div (Int l') (fromConstant (8 :: Natural))
      )

indexByteStringFun :: forall c. Sym c => Fun [BTByteString, BTInteger] BTInteger c
indexByteStringFun =
  withNumberOfRegisters @IntLength @Auto @(BaseField c) $
    withGetRegisterSize @IntLength @Auto @(BaseField c) $
      withCeilRegSize @(GetRegisterSize (BaseField c) IntLength Auto) @OrdWord $
        fromConstant
          ( \(VarByteString l b) i@(Int t) ->
              let
                indexIn = (isNotNegative i && t Symbolic.< fromConstant (value @BSLength))
                fr = from (resize t :: UInt (NumberOfBits (BaseField c)) Auto c)
                fe8 = fromConstant (8 :: Natural) :: FieldElement c
                newB = shiftR (shiftL b (fromConstant (value @BSLength) - l + fr * fe8)) (fromConstant (value @BSLength) - fe8)
               in
                bool Symbolic.nothing (Symbolic.just @c (Int . from $ truncate @_ @IntLength newB)) indexIn
          )

equalsByteStringFun :: forall c. Sym c => Fun '[BTByteString, BTByteString] BTBool c
equalsByteStringFun = fromConstant (\v w -> Symbolic.just @c $ bsBuffer v Symbolic.== bsBuffer w)

lessThanByteStringFun :: forall c. Sym c => Fun '[BTByteString, BTByteString] BTBool c
lessThanByteStringFun =
  withNumberOfRegisters @BSLength @Auto @(BaseField c) $
    withGetRegisterSize @BSLength @Auto @(BaseField c) $
      withCeilRegSize @(GetRegisterSize (BaseField c) BSLength Auto) @OrdWord $
        fromConstant (\v w -> Symbolic.just @c $ (from (bsBuffer v) :: UInt BSLength Auto c) Symbolic.< from (bsBuffer w))

lessThanEqualsByteStringFun :: forall c. Sym c => Fun '[BTByteString, BTByteString] BTBool c
lessThanEqualsByteStringFun =
  withNumberOfRegisters @BSLength @Auto @(BaseField c) $
    withGetRegisterSize @BSLength @Auto @(BaseField c) $
      withCeilRegSize @(GetRegisterSize (BaseField c) BSLength Auto) @OrdWord $
        fromConstant (\v w -> Symbolic.just @c $ (from (bsBuffer v) :: UInt BSLength Auto c) Symbolic.<= from (bsBuffer w))

--------------------------------------------------------------------------------
appendStringFun :: forall c. Sym c => Fun [BTString, BTString] BTString c
appendStringFun = fromConstant (\v w -> Symbolic.just @c $ dropZeros @StrLength (append v w))

equalsStringFun :: forall c. Sym c => Fun '[BTString, BTString] BTBool c
equalsStringFun = fromConstant (\v w -> Symbolic.just @c $ bsBuffer v Symbolic.== bsBuffer w)

encodeUtf8Fun :: forall c. Sym c => Fun '[BTString] BTByteString c
encodeUtf8Fun = fromConstant (\w -> Symbolic.just @c $ dropZeros @BSLength w)

decodeUtf8Fun :: forall c. Sym c => Fun '[BTByteString] BTString c
decodeUtf8Fun = fromConstant (\(VarByteString l b) -> Symbolic.just @c $ VarByteString l (resize b))

--------------------------------------------------------------------------------

sha2_256Fun :: forall c. Sym c => Fun '[BTByteString] BTByteString c
sha2_256Fun = fromConstant (\v -> Symbolic.just @c $ VarByteString (fromConstant (256 :: Natural)) (resize $ sha2Var @"SHA256" v))
