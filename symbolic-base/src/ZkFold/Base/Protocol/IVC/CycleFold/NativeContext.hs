{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Base.Protocol.IVC.CycleFold.NativeContext where

import           Control.Lens                                      ((^.))
import           Data.Function                                     (flip)
import           Data.Proxy                                        (Proxy (..))
import           GHC.Generics                                      (Par1 (..))
import           Prelude                                           (Functor (..), Integer, Traversable, const,
                                                                    type (~), undefined, ($), (.), (<$>))
import qualified Prelude                                           as Haskell
import           Test.QuickCheck                                   (Arbitrary (..))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field                   (Zp)
import           ZkFold.Base.Algebra.Basic.Number                  (KnownNat, Natural, type (+), type (-))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381       (BLS12_381_Scalar)
import           ZkFold.Base.Algebra.EllipticCurve.Class           (EllipticCurve (..))
import qualified ZkFold.Base.Algebra.EllipticCurve.Pasta           as Pasta
import           ZkFold.Base.Data.ByteString                       (Binary)
import           ZkFold.Base.Data.Package                          (unpacked)
import           ZkFold.Base.Data.Vector                           (Vector)
import           ZkFold.Base.Protocol.IVC.Accumulator
import           ZkFold.Base.Protocol.IVC.AccumulatorScheme        (AccumulatorScheme (..))
import           ZkFold.Base.Protocol.IVC.CycleFold.ForeignContext
import           ZkFold.Base.Protocol.IVC.NARK                     (NARKInstanceProof (..), NARKProof (..),
                                                                    narkInstanceProof)
import           ZkFold.Base.Protocol.IVC.Oracle
import           ZkFold.Symbolic.Class                             (Arithmetic, Symbolic (..), embedW)
import           ZkFold.Symbolic.Data.Bool                         (Bool, BoolType (..), false)
import           ZkFold.Symbolic.Data.Class                        (SymbolicData (..))
import           ZkFold.Symbolic.Data.Combinators                  (RegisterSize (..))
import           ZkFold.Symbolic.Data.FFA                          (KnownFFA)
import           ZkFold.Symbolic.Data.FieldElement                 (FieldElement (..))
import           ZkFold.Symbolic.Data.FieldElementW                (FieldElementW, unconstrainFieldElement)
import           ZkFold.Symbolic.Data.Pasta                        (PallasPoint, VestaPoint)
import           ZkFold.Symbolic.Interpreter                       (Interpreter)

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

data ForeignPoint algo (d :: Natural) k ctx = ForeignPoint
    { fpValue               :: ForeignOperation ctx
    , fpAccumulatorInstance :: AccumulatorInstance k (PredicateLayout (BaseField ctx)) (SecGroupLayout (BaseField ctx)) (FieldElement ctx)
    , fpAccumulatorProof    :: Vector k [FieldElement ctx]
    }

fpIsValid :: ForeignPoint algo d k ctx -> Bool ctx
fpIsValid = undefined

--------------------------------------------------------------------------------

toWitness ::
  forall t ctx . (Functor t, Symbolic ctx) =>
  t (FieldElement ctx) -> t (FieldElementW ctx)
toWitness = fmap unconstrainFieldElement

fromWitness :: forall t ctx . (Traversable t, Symbolic ctx) => t (WitnessField ctx) -> t (FieldElement ctx)
fromWitness = fmap FieldElement . unpacked . embedW

--------------------------------------------------------------------------------

fopCircuit :: forall algo d k a' ctx .
    -- BaseField ctx ~ a' ~ ScalarFieldOf Pallas
    ( HashAlgorithm algo
    , KnownNat (d-1)
    , KnownNat (d+1)
    , k ~ 1
    , Arithmetic a'
    , Binary a'
    , Symbolic ctx
    , FromConstant a' (ForeignField ctx)
    , Scale a' (ForeignField ctx)
    , BaseField ctx ~ a'
    , KnownFFA Pasta.FpModulus (Fixed 1) ctx
    , KnownFFA Pasta.FqModulus (Fixed 1) ctx
    )
    => ForeignPoint algo d k ctx
    -> ForeignOperationInput ctx
    -> ForeignPoint algo d k ctx
fopCircuit ForeignPoint {..} op =
    let
        -- witness computation

        g :: ForeignGroup ctx
        g = opRes fpValue

        input0 :: PredicateLayout a' (FieldElementW ctx)
        input0 = fmap (unconstrainFieldElement . FieldElement) $ unpacked $ flip arithmetize Proxy $
            NativeOperation zero zero zero (zero :: ForeignGroup ctx) false

        payload0 :: PredicatePayload a' (FieldElementW ctx)
        payload0 = fmap (unconstrainFieldElement . FieldElement) $ unpacked $ flip arithmetize Proxy $ case op of
            Addition h       -> NativePayload zero g h    false
            Multiplication s -> NativePayload s    g zero true

        -- This one should be in the foreign context!
        -- (that is, computation field is BaseFieldOf Pallas)
        narkIP :: NARKInstanceProof k (PredicateLayout a') (SecGroupLayout a') (FieldElementW ctx)
        narkIP@(NARKInstanceProof input (NARKProof piX _)) = narkInstanceProof (opProtocol @algo @d) input0 payload0

        accX :: AccumulatorInstance k (PredicateLayout a') (SecGroupLayout a') (FieldElementW ctx)
        accX = toWitness fpAccumulatorInstance

        accW :: Vector k [FieldElementW ctx]
        accW = fmap (fmap unconstrainFieldElement) fpAccumulatorProof

        acc :: Accumulator k (PredicateLayout a') (SecGroupLayout a') (FieldElement ctx)
        acc = Accumulator accX accW

        (acc', pf) = prover (opAccumulatorScheme @algo @_ @_ @a') acc narkIP

        -- in-circuit computation for ctx (inside Pallas scalar field)

        inputC :: NativeOperation (ForeignGroup ctx)
        inputC = _ -- fromWitness input

        piXC :: Vector k (SecGroup ctx)
        piXC = _ -- fromWitness piX

        accXC :: AccumulatorInstance k (PredicateLayout a') (SecGroupLayout a') (ForeignField ctx)
        accXC = fpAccumulatorInstance

        pfC :: Vector (d-1) (SecGroup ctx)
        pfC = _ -- fromWitness pf

        -- verifier is called in _ctx_ with @f ~ ScalarField Vesta@
        accX' :: AccumulatorInstance k (PredicateLayout a') (SecGroupLayout a') (ForeignField ctx)
        accX' = _ -- verifier (opAccumulatorScheme @algo @_ @_ @a') @(ForeignField ctx) inputC piXC accXC pfC

        accW' :: Vector k [ForeignField ctx]
        accW' = acc'^.w

    in ForeignPoint inputC accX' accW'

--------------------------------------------------------------------------------

instance Haskell.Show (ForeignPoint algo d k ctx) where
    show = const "ForeignPoint"

instance (Haskell.Eq (ctx Par1), Haskell.Eq (WitnessField ctx)) => Haskell.Eq (ForeignPoint algo d k ctx) where
    p == p' = opRes (fpValue p) Haskell.== opRes (fpValue p')

instance
    ( HashAlgorithm algo
    , KnownNat (d-1)
    , KnownNat (d+1)
    , k ~ 1
    , Symbolic ctx
    ) => AdditiveSemigroup (ForeignPoint algo d k ctx) where
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
    , Symbolic ctx
    ) => Scale Natural (ForeignPoint algo d k ctx) where
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
    , Symbolic ctx
    ) => AdditiveMonoid (ForeignPoint algo d k ctx) where
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
    , Symbolic ctx
    ) => Scale Integer (ForeignPoint algo d k ctx) where
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
    , Symbolic ctx
    ) => AdditiveGroup (ForeignPoint algo d k ctx) where
    negate = scale (-1 :: Integer)

instance
    ( HashAlgorithm algo
    , KnownNat (d-1)
    , KnownNat (d+1)
    , k ~ 1
    , Symbolic ctx
    ) => Arbitrary (ForeignPoint algo d k ctx) where
    arbitrary = do
        let acc = emptyAccumulator
        s <- fromConstant . toConstant @(Zp BLS12_381_Scalar) <$> arbitrary
        _
        -- return $ ForeignPoint (Comp1 $ opInit s) (Comp1 $ acc^.x) (toWitness @_ @ctx $ Comp1 $ Comp1 $ acc^.w)
