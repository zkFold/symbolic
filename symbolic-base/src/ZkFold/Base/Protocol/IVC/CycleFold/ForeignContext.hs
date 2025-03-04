{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Base.Protocol.IVC.CycleFold.ForeignContext where

import           Data.Foldable                              (Foldable)
import           Data.Function                              (const)
import           Data.Proxy                                 (Proxy (..))
import           GHC.Generics                               (Generic)
import           Prelude                                    (return, type (~), ($))
import qualified Prelude                                    as Haskell
import           Test.QuickCheck                            (Arbitrary (..))

import           ZkFold.Base.Algebra.Basic.Class            (AdditiveSemigroup (..), Scale (..), zero, (+))
import           ZkFold.Base.Algebra.Basic.Number           (KnownNat, type (+), type (-))
import           ZkFold.Base.Algebra.EllipticCurve.Class
import qualified ZkFold.Base.Algebra.EllipticCurve.Pasta    as Pasta
import           ZkFold.Base.Data.ByteString                (Binary)
import           ZkFold.Base.Protocol.IVC.AccumulatorScheme (AccumulatorScheme (..), accumulatorScheme)
import           ZkFold.Base.Protocol.IVC.CommitOpen        (commitOpen)
import           ZkFold.Base.Protocol.IVC.FiatShamir        (FiatShamir, fiatShamir)
import           ZkFold.Base.Protocol.IVC.Oracle
import           ZkFold.Base.Protocol.IVC.Predicate         (Predicate (..), predicate, PredicateFunction)
import           ZkFold.Base.Protocol.IVC.SpecialSound      (specialSoundProtocol)
import           ZkFold.Symbolic.Class                      (Arithmetic)
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators           (RegisterSize (..))
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.FFA                   (KnownFFA)
import           ZkFold.Symbolic.Data.Pasta                 (PallasPoint)
import           ZkFold.Symbolic.Interpreter                (Interpreter)

data NativeOperationInput point
  = Addition point
  | Multiplication (ScalarFieldOf point)

--------------------------------------------------------------------------------

data NativeOperation point = NativeOperation
    { opS   :: ScalarFieldOf point
    , opG   :: point
    , opH   :: point
    , opRes :: point
    , opId  :: BooleanOf point
    }
    deriving (Generic)

instance ( SymbolicData (ScalarFieldOf point)
         , SymbolicData (BooleanOf point)
         , SymbolicData point
         , Context point ~ Context (ScalarFieldOf point)
         , Context point ~ Context (BooleanOf point)
         , Support point ~ Support (ScalarFieldOf point)
         , Support point ~ Support (BooleanOf point)
         ) => SymbolicData (NativeOperation point)

instance Haskell.Eq point => Haskell.Eq (NativeOperation point) where
    op1 == op2 = opRes op1 Haskell.== opRes op2

opInit :: (CyclicGroup point, Eq point) => point -> NativeOperation point
opInit init = NativeOperation
    { opS   = zero
    , opG   = zero
    , opH   = zero
    , opRes = init
    , opId  = false
    }

instance
    ( Arbitrary (ScalarFieldOf point)
    , Arbitrary point
    , CyclicGroup point
    , Eq point
    ) => Arbitrary (NativeOperation point) where
    arbitrary = do
        s <- arbitrary
        g <- arbitrary
        h <- arbitrary
        op <- arbitrary
        return $ if op
            then NativeOperation s g h (g + h) false
            else NativeOperation s g h (scale s g) true

--------------------------------------------------------------------------------

data NativePayload point =
  NativePayload (ScalarFieldOf point) point point (BooleanOf point)
    deriving (Generic)

instance ( SymbolicData (ScalarFieldOf point)
         , SymbolicData (BooleanOf point)
         , SymbolicData point
         , Context point ~ Context (ScalarFieldOf point)
         , Context point ~ Context (BooleanOf point)
         , Support point ~ Support (ScalarFieldOf point)
         , Support point ~ Support (BooleanOf point)
         ) => SymbolicData (NativePayload point)

instance
    ( Arbitrary (ScalarFieldOf point)
    , Arbitrary point
    , Eq point
    ) => Arbitrary (NativePayload point) where
    arbitrary = do
        s <- arbitrary
        g <- arbitrary
        h <- arbitrary
        op <- arbitrary
        return $ NativePayload s g h (if op then false else true)

--------------------------------------------------------------------------------

opCircuit :: forall point. (CyclicGroup point, Eq point)
    => NativeOperation point
    -> NativePayload point
    -> NativeOperation point
opCircuit _ (NativePayload s g h op) =
    let
        opS :: ScalarFieldOf point
        opS = s

        opG :: point
        opG = g

        opH :: point
        opH = h

        opId :: BooleanOf point
        opId = op

        opRes :: point
        opRes = ifThenElse opId (scale opS opG) (opG + opH)
    in NativeOperation {..}

type PrimaryGroup c = PallasPoint c
type PredicateLayout a = Layout (NativeOperation (PrimaryGroup (Interpreter a)))
type PredicatePayload a = Layout (NativePayload (PrimaryGroup (Interpreter a)))

opPredicate :: forall a.
    ( Arithmetic a
    , Binary a
    , KnownFFA Pasta.FpModulus (Fixed 1) (Interpreter a)
    , KnownFFA Pasta.FqModulus (Fixed 1) (Interpreter a)
    )
    => Predicate a (PredicateLayout a) (PredicatePayload a)
opPredicate =
    let
        circuit :: PredicateFunction a (PredicateLayout a) (PredicatePayload a)
        circuit (x :: c (PredicateLayout a)) u =
            let
                x' = restore0 $ const x :: NativeOperation (PrimaryGroup c)
                u' = restore0 $ const u
            in arithmetize (opCircuit x' u') Proxy
    in
        predicate circuit

opProtocol :: forall algo d k a f.
    ( HashAlgorithm algo
    , KnownNat (d + 1)
    , k ~ 1
    , Arithmetic a
    , Binary a
    , KnownFFA Pasta.FpModulus (Fixed 1) (Interpreter a)
    , KnownFFA Pasta.FqModulus (Fixed 1) (Interpreter a)
    , Foldable f
    )
    => FiatShamir k a (PredicateLayout a) (PredicatePayload a) f
opProtocol = fiatShamir @algo $ commitOpen $ specialSoundProtocol @d $ opPredicate

opAccumulatorScheme :: forall algo d k a f.
    ( HashAlgorithm algo
    , KnownNat (d - 1)
    , KnownNat (d + 1)
    , Arithmetic a
    , Binary a
    , KnownFFA Pasta.FpModulus (Fixed 1) (Interpreter a)
    , KnownFFA Pasta.FqModulus (Fixed 1) (Interpreter a)
    , Foldable f
    )
    => AccumulatorScheme d k a (PredicateLayout a) f
opAccumulatorScheme = accumulatorScheme @algo $ opPredicate
