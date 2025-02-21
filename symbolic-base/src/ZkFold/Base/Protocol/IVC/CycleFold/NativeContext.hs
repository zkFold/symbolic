{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Base.Protocol.IVC.CycleFold.NativeContext where

import           Control.Lens                                      ((^.))
import           GHC.Generics                                      (Par1 (..), (:.:) (..))
import           Prelude                                           (Foldable, Functor (..), Integer, Traversable, const,
                                                                    return, type (~), undefined, ($), (.), (<$>))
import qualified Prelude                                           as Haskell
import           Test.QuickCheck                                   (Arbitrary (..))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field                   (Zp)
import           ZkFold.Base.Algebra.Basic.Number                  (KnownNat, Natural, type (+), type (-))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381       (BLS12_381_Scalar)
import           ZkFold.Base.Data.ByteString                       (Binary)
import           ZkFold.Base.Data.Package                          (packed, unpacked)
import           ZkFold.Base.Data.Vector                           (Vector)
import           ZkFold.Base.Protocol.IVC.Accumulator
import           ZkFold.Base.Protocol.IVC.AccumulatorScheme        (AccumulatorScheme (..))
import           ZkFold.Base.Protocol.IVC.CycleFold.ForeignContext
import           ZkFold.Base.Protocol.IVC.NARK                     (NARKInstanceProof (..), NARKProof (..),
                                                                    narkInstanceProof)
import           ZkFold.Base.Protocol.IVC.Oracle
import           ZkFold.Symbolic.Class                             (Arithmetic, Symbolic (..), embedW)
import           ZkFold.Symbolic.Data.Bool                         (Bool)
import           ZkFold.Symbolic.Data.FieldElement                 (FieldElement (..))
import ZkFold.Symbolic.Data.Ed25519 (Ed25519_Point)
import ZkFold.Base.Algebra.EllipticCurve.Class (EllipticCurve(..))
import ZkFold.Symbolic.Interpreter (Interpreter)
import ZkFold.Symbolic.Data.Class (SymbolicData(..))
import Data.Function (flip)
import ZkFold.Symbolic.Data.Pasta (PallasPoint, VestaPoint)

type ForeignGroup c = PallasPoint c
-- ^ The same point as 'PrimaryGroup', but viewed from native context:
-- base field is FFA(q), scalar field is FE(=Zp)
type ForeignField c = BaseFieldOf (ForeignGroup c)
-- ^ The intended behavior is actually to _not_ record any constraints,
-- as far as I can tell. For optimal implementation we need to
-- either introduce a "FFAW" which work in the same vein as 'FieldElementW'
-- or wait for Symbolic refactor
type ForeignOperationInput c = NativeOperationInput (ForeignGroup c)
type ForeignOperation c = NativeOperation (ForeignGroup c)
type ForeignPayload c = NativePayload (ForeignGroup c)

type SecGroup c = VestaPoint c
-- ^ The secondary group, but viewed from a foreign context:
-- base field is FFA(p), scalar field is FE(=Zq)
type SecGroupLayout a = Layout (SecGroup (Interpreter a))

--------------------------------------------------------------------------------

data ForeignPoint algo (d :: Natural) k a' ctx = ForeignPoint
    { fpValue               :: ForeignOperation ctx
    , fpAccumulatorInstance :: AccumulatorInstance k (PredicateLayout a') (SecGroupLayout a') (ForeignField ctx)
    , fpAccumulatorProof    :: Vector k [ForeignField ctx]
    }

fpIsValid :: ForeignPoint algo d k a' ctx -> Bool ctx
fpIsValid = undefined

--------------------------------------------------------------------------------

toWitness :: forall t ctx . (Functor t, Foldable t, Symbolic ctx) => t (FieldElement ctx) -> t (WitnessField ctx)
toWitness = witnessF . packed . fmap fromFieldElement

fromWitness :: forall t ctx . (Traversable t, Symbolic ctx) => t (WitnessField ctx) -> t (FieldElement ctx)
fromWitness = fmap FieldElement . unpacked . embedW

--------------------------------------------------------------------------------

fopCircuit :: forall algo d k a' ctx .
    ( HashAlgorithm algo
    , KnownNat (d-1)
    , KnownNat (d+1)
    , k ~ 1
    , Arithmetic a'
    , Binary a'
    , Symbolic ctx
    , FromConstant a' (ForeignField ctx)
    , Scale a' (ForeignField ctx)
    )
    => ForeignPoint algo d k a' ctx
    -> ForeignOperationInput ctx
    -> ForeignPoint algo d k a' ctx
fopCircuit ForeignPoint {..} op =
    let
        -- witness computation

        g :: Layout (PrimaryGroup (Interpreter a')) a'
        g = _ -- toWitness $ opRes fpValue

        input0 :: PredicateLayout a' a'
        input0 = NativeOperation zero zero zero zero zero

        payload :: PredicatePayload a' a'
        payload = flip arithmetize Proxy $ case op of
            Addition h       -> NativePayload zero _ {- g -} h    zero
            Multiplication s -> NativePayload s    _ {- g -} zero one

        narkIP :: NARKInstanceProof k (PredicateLayout a') (SecGroupLayout a') (ForeignField ctx)
        narkIP@(NARKInstanceProof input (NARKProof piX _)) = _ -- narkInstanceProof (opProtocol @algo @d @_ @a') input0 payload

        accX :: AccumulatorInstance k (PredicateLayout a') (SecGroupLayout a') (ForeignField ctx)
        accX = _ -- toWitness fpAccumulatorInstance

        accW :: Vector k [ForeignField ctx]
        accW = fpAccumulatorProof

        acc :: Accumulator k (PredicateLayout a') (SecGroupLayout a') (ForeignField ctx)
        acc = Accumulator accX accW

        (acc', pf) = prover (opAccumulatorScheme @algo @_ @_ @a') acc narkIP

        -- in-circuit computation

        inputC :: NativeOperation (ForeignGroup ctx)
        inputC = _ -- fromWitness input

        piXC :: Vector k (SecGroup ctx)
        piXC = _ -- fromWitness piX

        accXC :: AccumulatorInstance k (PredicateLayout a') (SecGroupLayout a') (ForeignField ctx)
        accXC = fpAccumulatorInstance

        pfC :: Vector (d-1) (SecGroup ctx)
        pfC = _ -- fromWitness pf

        accX' :: AccumulatorInstance k (PredicateLayout a') (SecGroupLayout a') (ForeignField ctx)
        accX' = _ -- verifier (opAccumulatorScheme @algo @_ @_ @a') @(ForeignField ctx) inputC piXC accXC pfC

        accW' :: Vector k [ForeignField ctx]
        accW' = acc'^.w

    in ForeignPoint inputC accX' accW'

--------------------------------------------------------------------------------

instance Haskell.Show (ForeignPoint algo d k a' ctx) where
    show = const "ForeignPoint"

instance (Haskell.Eq (ctx Par1), Haskell.Eq (WitnessField ctx)) => Haskell.Eq (ForeignPoint algo d k a' ctx) where
    p == p' = opRes (fpValue p) Haskell.== opRes (fpValue p')

instance
    ( HashAlgorithm algo
    , KnownNat (d-1)
    , KnownNat (d+1)
    , k ~ 1
    , Arithmetic a'
    , Binary a'
    , Symbolic ctx
    , FromConstant a' (ForeignField ctx)
    , Scale a' (ForeignField ctx)
    ) => AdditiveSemigroup (ForeignPoint algo d k a' ctx) where
    p + p' =
        let
            g :: ForeignGroup ctx
            g = _

            opi :: ForeignOperationInput ctx
            opi = _
        in
            fopCircuit p opi

instance
    ( HashAlgorithm algo
    , KnownNat (d-1)
    , KnownNat (d+1)
    , k ~ 1
    , Arithmetic a'
    , Binary a'
    , Symbolic ctx
    , FromConstant a' (ForeignField ctx)
    , Scale a' (ForeignField ctx)
    ) => Scale Natural (ForeignPoint algo d k a' ctx) where
    scale a p =
        let
            s :: ForeignField ctx
            s = fromConstant a

            opi :: ForeignOperationInput ctx
            opi = Multiplication _
        in
            fopCircuit p opi

instance
    ( HashAlgorithm algo
    , KnownNat (d-1)
    , KnownNat (d+1)
    , k ~ 1
    , Arithmetic a'
    , Binary a'
    , Symbolic ctx
    , FromConstant a' (ForeignField ctx)
    , Scale a' (ForeignField ctx)
    ) => AdditiveMonoid (ForeignPoint algo d k a' ctx) where
    zero =
        let
            acc = emptyAccumulator
        in
            _
            -- ForeignPoint (Comp1 $ opInit zero) (Comp1 $ acc^.x) (toWitness @_ @ctx $ Comp1 $ Comp1 $ acc^.w)

instance
    ( HashAlgorithm algo
    , KnownNat (d-1)
    , KnownNat (d+1)
    , k ~ 1
    , Arithmetic a'
    , Binary a'
    , Symbolic ctx
    , FromConstant a' (ForeignField ctx)
    , Scale a' (ForeignField ctx)
    ) => Scale Integer (ForeignPoint algo d k a' ctx) where
    scale a p =
        let
            s :: ForeignField ctx
            s = fromConstant a

            opi :: ForeignOperationInput ctx
            opi = Multiplication _
        in
            fopCircuit p opi

instance
    ( HashAlgorithm algo
    , KnownNat (d-1)
    , KnownNat (d+1)
    , k ~ 1
    , Arithmetic a'
    , Binary a'
    , Symbolic ctx
    , FromConstant a' (ForeignField ctx)
    , Scale a' (ForeignField ctx)
    ) => AdditiveGroup (ForeignPoint algo d k a' ctx) where
    negate = scale (-1 :: Integer)

instance
    ( HashAlgorithm algo
    , KnownNat (d-1)
    , KnownNat (d+1)
    , k ~ 1
    , Arithmetic a'
    , Binary a'
    , Symbolic ctx
    , FromConstant a' (ForeignField ctx)
    , Scale a' (ForeignField ctx)
    ) => Arbitrary (ForeignPoint algo d k a' ctx) where
    arbitrary = do
        let acc = emptyAccumulator
        s <- fromConstant . toConstant @(Zp BLS12_381_Scalar) <$> arbitrary
        _
        -- return $ ForeignPoint (Comp1 $ opInit s) (Comp1 $ acc^.x) (toWitness @_ @ctx $ Comp1 $ Comp1 $ acc^.w)
