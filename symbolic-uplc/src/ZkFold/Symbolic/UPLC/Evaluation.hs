{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module ZkFold.Symbolic.UPLC.Evaluation (Sym, ExValue (..), MaybeValue (..), eval) where

import           Control.Monad                    (return)
import           Data.Either                      (Either (..))
import           Data.Function                    (($), (.))
import           Data.Functor                     ((<$>))
import           Data.Functor.Rep                 (Representable)
import           Data.List                        (map, null, (++))
import           Data.Maybe                       (Maybe (..))
import           Data.Ord                         ((<))
import           Data.Proxy                       (Proxy (..))
import           Data.Traversable                 (Traversable, traverse)
import           Data.Typeable                    (Typeable, cast)
import           Prelude                          (error, fromIntegral)

import           ZkFold.Base.Algebra.Basic.Class  (FromConstant (..), (+))
import           ZkFold.Prelude                   ((!!))
import           ZkFold.Symbolic.Class            (Symbolic)
import           ZkFold.Symbolic.Data.Bool        (Bool, BoolType (..))
import           ZkFold.Symbolic.Data.Class       (SymbolicData (..))
import           ZkFold.Symbolic.Data.Conditional (Conditional, bool)
import qualified ZkFold.Symbolic.Data.Maybe       as Symbolic
import qualified ZkFold.Symbolic.UPLC.Data        as Symbolic
import           ZkFold.UPLC.BuiltinFunction
import           ZkFold.UPLC.BuiltinType
import           ZkFold.UPLC.Term

------------------------------- MAIN ALGORITHM ---------------------------------

-- This part is not meant to be changed when extending the Converter
-- (except for bugfixing, of course).

-- | Class of Symbolic datatypes used inside Converter.
-- Each instance enforces a one-to-one correspondence between some 'BuiltinType'
-- and its interpretation as a Symbolic datatype in arbitrary context 'c'.
class
    ( Typeable v, SymbolicData v
    , Context v ~ c, Support v ~ Proxy c
    -- TODO: Remove after Conditional becomes part of SymbolicData
    , Conditional (Bool c) v
    , Representable (Layout v)
    , Traversable (Layout v)
    , Representable (Payload v)
    ) => IsData (t :: BuiltinType) v c | t c -> v, v -> t, v -> c where
  asPair :: v -> Maybe (ExValue c, ExValue c)

-- | Existential wrapper around 'IsData' Symbolic types.
data ExValue c = forall t v. IsData t v c => ExValue v

-- | We can evaluate UPLC terms in arbitrary 'Symbolic' context as long as
-- it is also 'Typeable'.
type Sym c = (Symbolic c, Typeable c)

-- | According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf),
-- evaluation of a UPLC term is a partial function.
--
-- In addition, successful evaluation of a smart contract compiled to UPLC
-- should yield a value of a builtin type.
--
-- We encode this with a Symbolic 'Maybe' of an arbitrary 'IsData'.
data MaybeValue c = forall t v. IsData t v c => MaybeValue (Symbolic.Maybe c v)

-- | Evaluation function.
--
-- Given enough arguments, we can evaluate a first-order
-- (that is, not taking functions as arguments)
-- UPLC term into a value of builtin type.
eval :: forall c. Sym c => Term -> [ExValue c] -> MaybeValue c
eval t args = case impl [] t $ map aValue args of
  Just v  -> v
  Nothing -> MaybeValue (Symbolic.nothing :: Symbolic.Maybe c (Proxy c))

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
impl ::
  Sym c =>
  Env c -> -- ^ Evaluation environment (thunks accessed by De Bruijn indices).
  Term -> -- ^ Term in question.
  [Arg c] -> -- ^ List of arguments to apply to the term.
  SomeValue c -- ^ Result of an evaluation.
impl env (TVariable i) args            = applyVar env i args -- inspect environment
impl _   (TConstant c) []              = someValue (evalConstant c) -- embed constant inside context
impl _   (TConstant _) (_:_)           = Nothing -- constants are not functions!
impl env (TBuiltin f)  args            = applyBuiltin env f args -- inspect builtin
impl _   (TLam _)      []              = Nothing -- not enough arguments supplied!
impl _   (TLam _)      (ACase _:_)     = Nothing -- lambda cannot be pattern-matched!
impl env (TLam f)      (AThunk t:args) = beta t env f args -- eval body
impl env (TApp f x)    args            = impl env f (aTerm x : args) -- prepend new arg for eval of a function
impl env (TDelay t)    args            = impl env t args -- we skip delays. Maybe wrong, but simpler
impl env (TForce t)    args            = impl env t args -- we skip forcings. Maybe wrong, but simpler.
impl env (TConstr t f) (ACase bs:args) = impl env (bs !! fromIntegral t) (map aTerm f ++ args) -- pattern-matching
impl env (TConstr t f) []              = constr t <$> traverse (\fi -> impl env fi []) f -- embed constructor
impl _   (TConstr _ _) (_:_)           = Nothing -- constructors are not functions!
impl env (TCase s bs)  args            = impl env s (ACase bs : args) -- defer pattern-matching
impl _   TError        _               = Nothing -- errors are errors!

-- | Inspect the thunk pointed at by the variable.
applyVar :: Sym c => Env c -> DeBruijnIndex -> [Arg c] -> SomeValue c
applyVar ctx i args = case ctx !! i of
  Left t  -> impl ctx t args -- evaluate the term
  Right v -> if null args then v else Nothing -- data are not functions!

-- | Prepares context and arguments to enter new scope, and evaluates the body.
-- Classic stuff when working with de Bruijn indices.
beta :: Sym c => Thunk c -> Env c -> Term -> [Arg c] -> SomeValue c
beta e env t args = impl (e : map shiftT env) t (map shiftA args)
  where
    shiftA (ACase ts)     = ACase (map (`shift` 0) ts)
    shiftA (AThunk thunk) = AThunk (shiftT thunk)
    shiftT (Left term)   = Left (shift term 0)
    shiftT (Right value) = Right value
    shift (TVariable i) b        = TVariable (i + if i < b then 0 else 1)
    shift (TConstant c) _        = TConstant c
    shift (TBuiltin f) _         = TBuiltin f
    shift (TLam body) b          = TLam $ shift body (b + 1)
    shift (TApp fun arg) b       = TApp (shift fun b) (shift arg b)
    shift (TDelay term) b        = TDelay (shift term b)
    shift (TForce term) b        = TForce (shift term b)
    shift (TConstr tag fields) b = TConstr tag $ map (`shift` b) fields
    shift (TCase scr brs) b      = TCase (shift scr b) $ map (`shift` b) brs
    shift TError _               = TError

-- | Inspect the builtin.
applyBuiltin :: Sym c => Env c -> BuiltinFunction s t -> [Arg c] -> SomeValue c
applyBuiltin ctx (BFMono f) args = applyMono true (evalMono f) [evalArg ctx a [] | a <- args]
applyBuiltin ctx (BFPoly f) args = applyPoly ctx f args

-- | Try to turn the argument into a value.
evalArg :: Sym c => Env c -> Arg c -> [Arg c] -> SomeValue c
evalArg _   (ACase _)          _     = Nothing
evalArg ctx (AThunk (Left t))  args  = impl ctx t args
evalArg _   (AThunk (Right v)) []    = v
evalArg _   (AThunk (Right _)) (_:_) = Nothing

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
applyMono b (FSat x) []       = Just $ MaybeValue (bool Symbolic.nothing x b)
applyMono _ (FSat _) (_:_)    = Nothing
applyMono _ (FLam _) []       = Nothing
applyMono b (FLam f) (v:args) = do
  MaybeValue v' <- v
  mx <- cast v'
  applyMono (b && Symbolic.isJust mx) (f $ Symbolic.fromJust mx) args

--------------------------- BUILTINS INTERPRETATION ----------------------------

-- This part is meant to be changed when completing the Converter implementation.

-- Uncomment these lines as more types are available in Converter:
-- instance Sym c => IsData BTInteger ??? c where asPair _ = Nothing
-- instance Sym c => IsData BTByteString ??? c where asPair _ = Nothing
-- instance Sym c => IsData BTString ??? c where asPair _ = Nothing
instance Sym c => IsData BTBool (Bool c) c where asPair _ = Nothing
instance Sym c => IsData BTUnit (Proxy c) c where asPair _ = Nothing
instance Sym c => IsData BTData (Symbolic.Data c) c where asPair _ = Nothing
-- Uncomment this line as List becomes available in Converter:
-- instance (Sym c, IsData t v c) => IsData (BTList t) ??? c where asPair _ = Nothing
instance
  (Sym c, IsData t v c, IsData t' v' c) => IsData (BTPair t t') (v, v') c where
  asPair (p, q) = Just (ExValue p, ExValue q)
-- Uncomment these lines as more types are available in Converter:
-- instance Sym c => IsData BTBLSG1 ??? c where asPair _ = Nothing
-- instance Sym c => IsData BTBLSG2 ??? c where asPair _ = Nothing
-- instance Sym c => IsData BTBLSMLResult ??? c where asPair _ = Nothing


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
applyPoly ctx IfThenElse (ct:tt:et:args) = do
  MaybeValue c0 <- evalArg ctx ct []
  withArms (evalArg ctx tt args) (evalArg ctx et args) $ \t e0 -> do
    c :: Symbolic.Maybe c (Bool c) <- cast c0
    e <- cast e0
    return $ MaybeValue (Symbolic.maybe Symbolic.nothing (bool e t) c)
applyPoly ctx ChooseUnit (_:t:args) = evalArg ctx t args
applyPoly ctx Trace (_:t:args) = evalArg ctx t args
applyPoly ctx FstPair [arg] = do
  MaybeValue p <- evalArg ctx arg []
  (ExValue v, _) <- asPair (Symbolic.fromJust p)
  return $ MaybeValue (symMaybe p v)
applyPoly ctx SndPair [arg] = do
  MaybeValue p <- evalArg ctx arg []
  (_, ExValue v) <- asPair (Symbolic.fromJust p)
  return $ MaybeValue (symMaybe p v)
applyPoly _ (BPFList _) _ = error "FIXME: UPLC List support"
applyPoly _ ChooseData _ = error "FIXME: UPLC Data support"
applyPoly _ _ _ = Nothing

-- | Correct error propagation for if-then-else
withArms ::
  Sym c => SomeValue c -> SomeValue c ->
  ( forall s t u v. (IsData s u c, IsData t v c) =>
    Symbolic.Maybe c u -> Symbolic.Maybe c v -> Maybe r
  ) -> Maybe r
withArms (Just (MaybeValue t)) (Just (MaybeValue e0)) f = f t e0
withArms (Just (MaybeValue @_ @_ @u t)) Nothing f       = f t (Symbolic.nothing @u)
withArms Nothing (Just (MaybeValue @_ @_ @v e0)) f      = f (Symbolic.nothing @v) e0
withArms Nothing Nothing _                              = Nothing

-- | Helper function.
symMaybe :: (Sym c, IsData d t c) => Symbolic.Maybe c u -> t -> Symbolic.Maybe c t
symMaybe b v = bool Symbolic.nothing (Symbolic.just v) (Symbolic.isJust b)

-- | Some Symbolic value of a definite UPLC builtin type.
data SymValue t c = forall v. IsData t v c => SymValue v

-- | Given a UPLC constant, evaluate it as a corresponding Symbolic value.
-- Types would not let you go (terribly) wrong!
evalConstant :: Sym c => Constant t -> SymValue t c
evalConstant (CBool b)       = SymValue (if b then true else false)
evalConstant (CInteger _)    = error "FIXME: UPLC Integer support"
evalConstant (CByteString _) = error "FIXME: UPLC ByteString support"
evalConstant (CString _)     = error "FIXME: UPLC String support"
evalConstant (CUnit ())      = SymValue Proxy
evalConstant (CData _)       = error "FIXME: UPLC Data support"
evalConstant (CList _)       = error "FIXME: UPLC List support"
evalConstant (CPair p q)     = pair (evalConstant p) (evalConstant q)
evalConstant (CG1 _)         = error "FIXME: UPLC BLS support"
evalConstant (CG2 _)         = error "FIXME: UPLC BLS support"

-- | Useful helper function.
pair :: Sym c => SymValue t c -> SymValue u c -> SymValue (BTPair t u) c
pair (SymValue p) (SymValue q) = SymValue (p, q)

-- | Given a tag and fields, evaluate them as an instance of UPLC Data type.
constr :: Sym c => ConstructorTag -> [MaybeValue c] -> MaybeValue c
constr _ _ = error "FIXME: UPLC Data support"

-- | Symbolic function of a definite UPLC signature.
data Fun (s :: [BuiltinType]) (t :: BuiltinType) c where
  -- | Fully applied (saturated) function.
  FSat :: IsData t v c => Symbolic.Maybe c v -> Fun '[] t c
  -- | A function which returns another (possibly saturated) function.
  FLam :: IsData s v c => (v -> Fun ss t c) -> Fun (s ': ss) t c

instance IsData t v c => FromConstant (Symbolic.Maybe c v) (Fun '[] t c) where
  fromConstant = FSat

instance
    (IsData s v c, FromConstant f (Fun ss t c))
    => FromConstant (v -> f) (Fun (s ': ss) t c) where
  fromConstant f = FLam (fromConstant . f)

-- | Given a monomorphic UPLC builtin, evaluate it
-- as a corresponding Symbolic function.
--
-- Types will not let you go (terribly) wrong!
--
-- Note that you can use 'FromConstant' instances defined above
-- to get rid of the 'FSat'/'FLam' boilerplate.
evalMono :: Sym c => BuiltinMonoFunction s t -> Fun s t c
evalMono (BMFInteger _)    = error "FIXME: UPLC Integer support"
evalMono (BMFByteString _) = error "FIXME: UPLC ByteString support"
evalMono (BMFString _)     = error "FIXME: UPLC String support"
evalMono (BMFAlgorithm _)  = error "FIXME: UPLC Algorithms support"
evalMono (BMFData _)       = error "FIXME: UPLC Data support"
evalMono (BMFCurve _)      = error "FIXME: UPLC Curve support"
evalMono (BMFBitwise _)    = error "FIXME: UPLC ByteString support"
