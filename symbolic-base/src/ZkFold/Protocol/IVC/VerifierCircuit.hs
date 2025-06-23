{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Protocol.IVC.VerifierCircuit where

-- import           GHC.Generics                        (Par1 (..), U1 (..), type (:.:) (..), (:*:) (..))
-- import           Prelude                             hiding (Num (..), drop, head, replicate, take, zipWith)

-- import           ZkFold.Base.Algebra.Basic.Number    (KnownNat, type (+), type (-))
-- import           ZkFold.Base.Data.ByteString         (Binary)
-- import           ZkFold.Base.Data.Vector             (Vector)
-- import           ZkFold.Base.Protocol.IVC.Commit     (HomomorphicCommit)
-- import           ZkFold.Base.Protocol.IVC.CommitOpen (CommitOpen (..))
-- import           ZkFold.Base.Protocol.IVC.FiatShamir (FiatShamir (..))
-- import           ZkFold.Base.Protocol.IVC.Internal   (IVCResult, ivcVerify, RecursiveI, RecursiveP)
-- import           ZkFold.Base.Protocol.IVC.Oracle     (HashAlgorithm)
-- import           ZkFold.Base.Protocol.IVC.Predicate  (Predicate (..), predicate)
-- import           ZkFold.Symbolic.Class
-- import           ZkFold.Symbolic.Compiler
-- import           ZkFold.Symbolic.Data.Class          (SymbolicData (..))
-- import           ZkFold.Symbolic.Data.FieldElement   (FieldElement (..))
-- import           ZkFold.Symbolic.Data.Input          (SymbolicInput)
-- import ZkFold.Base.Protocol.IVC.SpecialSound (SpecialSoundProtocol(..))

-- -- | Takes a function `f` and returns a circuit `C` with input `y` and witness `w`.
-- -- The circuit is such that `C(y, w) = 0` implies that `y = x(n)` for some positive `n` where
-- -- `x(k+1) = f(x(k), u(k))` for all `k` and some `u`.
-- ivcVerifierCircuit :: forall f i p m c d k a payload input output nx nu ctx algo .
--     ( f ~ FieldElement ctx
--     , i ~ Vector nx
--     , p ~ Vector nu
--     , m ~ [f]
--     , c ~ f
--     , HashAlgorithm algo f
--     , HomomorphicCommit m c
--     , KnownNat (d - 1)
--     , KnownNat (d + 1)
--     , KnownNat k
--     , KnownNat (k - 1)
--     , Binary a
--     , Arithmetic a
--     , KnownNat nx
--     , KnownNat nu
--     , ctx ~ ArithmeticCircuit a payload input
--     , SymbolicInput (IVCResult f i m c k)
--     , Context (IVCResult f i m c k) ~ ctx
--     -- , payload ~ (Payload (IVCResult f i m c k) :*: U1)
--     -- , input ~ (Layout (IVCResult f i m c k) :*: U1)
--     -- , output ~ (((Par1 :*: (Vector nx :.: Par1)) :*: ((Vector (k - 1) :.: Par1) :*: ((Vector k :.: Par1) :*: Par1))) :*: ((Vector k :.: Par1) :*: Par1))
--     ) => (forall ctx' . Symbolic ctx' => Vector nx (FieldElement ctx') -> Vector nu (FieldElement ctx') -> Vector nx (FieldElement ctx'))
--     -> ArithmeticCircuit a payload input output
-- ivcVerifierCircuit func =
--     let
--         -- Protostar IVC takes an arithmetizable function as input.
--         p :: Predicate a (Vector nx) (Vector nu)
--         p = predicate func

--         -- The Fiat-Shamired commit-open special-sound protocol for the arithmetizable function
--         fs :: FiatShamir algo (CommitOpen (Predicate a (Vector nx) (Vector nu)))
--         fs = FiatShamir (CommitOpen p)

--         -- The verification function for the IVC result object
--         vf :: IVCResult f i m c k
--             -> (VerifierOutput f (RecursiveI i c k) (RecursiveP i p c d k) (Vector k (m, c)) c d 1 (FiatShamir algo (CommitOpen (Predicate a (Vector nx) (Vector nu)))), (Vector k c, c))
--         vf = ivcVerify @f @i @p @m @c @d @k p

--         -- -- TODO: the circuit functors must be adjusted for better usability
--         -- circuit = compile @a $ ivcVerify @f @i @p @m @c @d @k p
--         circuit = undefined
--     in circuit
