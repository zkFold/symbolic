{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples.UtxoAccumulator where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Function (const, flip, ($))
import Data.Functor (fmap)
import Data.Functor.Rep (tabulate)
import GHC.Generics (Generic, Par1 (..), U1 (..), (:*:) (..), (:.:) (..))
import ZkFold.Algebra.Class (fromConstant, one, toConstant, zero, (+), (-!))
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point, BLS12_381_G2_Point)
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Algebra.Number (KnownNat, value)
import ZkFold.Algebra.Polynomial.Univariate (PolyVec)
import ZkFold.Data.ByteString (Binary)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.HFunctor (hmap)
import ZkFold.Data.Vector (Vector, unsafeToVector)
import ZkFold.Prelude (length, replicate, take, (!!))
import ZkFold.Protocol.NonInteractiveProof (NonInteractiveProof (setupProve, setupVerify), prove)
import ZkFold.Protocol.Plonkup (Plonkup (..), PlonkupPolyExtendedLength)
import ZkFold.Protocol.Plonkup.Input (PlonkupInput)
import ZkFold.Protocol.Plonkup.Internal (lagrangeBasisGroupElements)
import ZkFold.Protocol.Plonkup.Proof (PlonkupProof)
import ZkFold.Protocol.Plonkup.Prover (PlonkupProverSecret (..), PlonkupProverSetup (..))
import ZkFold.Protocol.Plonkup.Update (updateProverSetup, updateVerifierSetup)
import ZkFold.Protocol.Plonkup.Utils (getParams)
import ZkFold.Protocol.Plonkup.Verifier (PlonkupVerifierSetup)
import ZkFold.Protocol.Plonkup.Witness (PlonkupWitnessInput (..))
import ZkFold.Symbolic.Algorithm.Hash.MiMC (hash)
import ZkFold.Symbolic.Class (Arithmetic, Symbolic)
import ZkFold.Symbolic.Compiler (compileWith)
import ZkFold.Symbolic.Compiler.ArithmeticCircuit (ArithmeticCircuit, solder)
import ZkFold.Symbolic.Data.Bool (Bool (..), all, any, (&&))
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Interpreter (Interpreter)
import Prelude ((++))

utxoAccumulator
  :: forall n c
   . Symbolic c
  => Vector n (FieldElement c)
  -> Vector n (FieldElement c)
  -> (FieldElement c, FieldElement c)
  -> (Bool c, Vector n (FieldElement c), Vector n (FieldElement c))
utxoAccumulator hs as (a, r) =
  let
    h = hash (a, r)

    cond1 = any (== h) hs
    cond2 = all (/= a) as
   in
    (cond1 && cond2, hs, as)

type UtxoAccumulatorInput n = Vector n :*: Vector n :*: Par1 :*: Par1

type UtxoAccumulatorOutput n = Par1 :*: Vector n :*: Vector n

utxoAccumulatorCircuit
  :: forall n a
   . (KnownNat n, Arithmetic a, Binary a)
  => ArithmeticCircuit a (UtxoAccumulatorInput n) (UtxoAccumulatorOutput n)
utxoAccumulatorCircuit =
  hmap (\(i1 :*: Comp1 i2 :*: Comp1 i3) -> i1 :*: fmap unPar1 i2 :*: fmap unPar1 i3)
    $ compileWith
      solder
      ( \(i1 :*: i2 :*: i3) ->
          ( Comp1 (tabulate $ const U1) :*: Comp1 (tabulate $ const U1) :*: (U1 :*: U1) :*: U1
          , Comp1 (fmap Par1 i1) :*: Comp1 (fmap Par1 i2) :*: i3 :*: U1
          )
      )
    $ utxoAccumulator @n

utxoAccumulatorInput
  :: forall n a
   . Vector n a
  -> Vector n a
  -> (a, a)
  -> UtxoAccumulatorInput n a
utxoAccumulatorInput hs as (a, r) =
  hs :*: as :*: (Par1 a :*: Par1 r)

data UtxoAccumulatorCRS = UtxoAccumulatorCRS
  { crsGs :: [BLS12_381_G1_Point]
  , crsHs :: [BLS12_381_G2_Point]
  , crsAccElems :: [BLS12_381_G1_Point]
  , crsDistElems :: [BLS12_381_G1_Point]
  }
  deriving (FromJSON, Generic, ToJSON)

type UtxoAccumulatorProtocol n m =
  Plonkup
    (UtxoAccumulatorInput n)
    (UtxoAccumulatorOutput n)
    m
    BLS12_381_G1_Point
    BLS12_381_G2_Point
    ByteString
    (PolyVec (ScalarFieldOf BLS12_381_G1_Point))

utxoAccumulatorProtocol
  :: forall n m
   . (KnownNat n, KnownNat m)
  => UtxoAccumulatorCRS
  -> UtxoAccumulatorProtocol n m
utxoAccumulatorProtocol crs =
  let
    (omega, k1, k2) = getParams (value @m)
    gs = unsafeToVector $ take (value @m + 6) (crsGs crs)
    h1 = crsHs crs !! 1
   in
    Plonkup omega k1 k2 utxoAccumulatorCircuit h1 gs

utxoAccumulatorProverSetup
  :: forall n m
   . (KnownNat n, KnownNat m, KnownNat (PlonkupPolyExtendedLength m))
  => UtxoAccumulatorCRS
  -> [ScalarFieldOf BLS12_381_G1_Point]
  -> [ScalarFieldOf BLS12_381_G1_Point]
  -> PlonkupProverSetup
       (UtxoAccumulatorInput n)
       (UtxoAccumulatorOutput n)
       m
       BLS12_381_G1_Point
       BLS12_381_G2_Point
       (PolyVec (ScalarFieldOf BLS12_381_G1_Point))
utxoAccumulatorProverSetup crs hs as =
  flip updateProverSetup (as ++ replicate (value @n -! length hs) zero) $
    flip updateProverSetup (hs ++ replicate (value @n -! length hs) zero) $
      flip updateProverSetup [one] $
        setupProve $
          utxoAccumulatorProtocol crs

utxoAccumulatorProverSetupInit
  :: forall n m
   . (KnownNat n, KnownNat m, KnownNat (PlonkupPolyExtendedLength m))
  => UtxoAccumulatorCRS
  -> PlonkupProverSetup
       (UtxoAccumulatorInput n)
       (UtxoAccumulatorOutput n)
       m
       BLS12_381_G1_Point
       BLS12_381_G2_Point
       (PolyVec (ScalarFieldOf BLS12_381_G1_Point))
utxoAccumulatorProverSetupInit crs = utxoAccumulatorProverSetup crs [] []

utxoAccumulatorHash
  :: ScalarFieldOf BLS12_381_G1_Point
  -> ScalarFieldOf BLS12_381_G1_Point
  -> ScalarFieldOf BLS12_381_G1_Point
utxoAccumulatorHash a r =
  let
    f = fromConstant @(ScalarFieldOf BLS12_381_G1_Point) @(FieldElement (Interpreter (ScalarFieldOf BLS12_381_G1_Point)))
   in
    toConstant $ hash (f a, f r)

utxoAccumulatorProve
  :: forall n m
   . (KnownNat n, KnownNat m, KnownNat (PlonkupPolyExtendedLength m))
  => UtxoAccumulatorCRS
  -> [ScalarFieldOf BLS12_381_G1_Point]
  -> [ScalarFieldOf BLS12_381_G1_Point]
  -> ScalarFieldOf BLS12_381_G1_Point
  -> ScalarFieldOf BLS12_381_G1_Point
  -> (PlonkupInput BLS12_381_G1_Point, PlonkupProof BLS12_381_G1_Point)
utxoAccumulatorProve crs hs as a r =
  let
    setup = utxoAccumulatorProverSetup crs hs as
    witness = PlonkupWitnessInput (unsafeToVector hs :*: unsafeToVector as :*: Par1 a :*: Par1 r)
    secret = PlonkupProverSecret $ tabulate (\k -> utxoAccumulatorHash r $ fromConstant $ toConstant k)
   in
    prove @(UtxoAccumulatorProtocol n m) setup (witness, secret)

utxoAccumulatorVerifierSetup
  :: forall n m
   . (KnownNat n, KnownNat m, KnownNat (PlonkupPolyExtendedLength m))
  => UtxoAccumulatorCRS
  -> PlonkupVerifierSetup
       (UtxoAccumulatorInput n)
       (UtxoAccumulatorOutput n)
       m
       BLS12_381_G1_Point
       BLS12_381_G2_Point
       (PolyVec (ScalarFieldOf BLS12_381_G1_Point))
utxoAccumulatorVerifierSetup crs =
  updateVerifierSetup (setupVerify $ utxoAccumulatorProtocol crs) [one] [validationGroupElement @n @m crs]

utxoAccumulatorGroupElements
  :: forall n m
   . (KnownNat n, KnownNat m, KnownNat (PlonkupPolyExtendedLength m))
  => UtxoAccumulatorCRS
  -> [BLS12_381_G1_Point]
utxoAccumulatorGroupElements crs =
  let
    PlonkupProverSetup {..} = utxoAccumulatorProverSetupInit @n @m crs
   in
    lagrangeBasisGroupElements @m @BLS12_381_G1_Point @(PolyVec (ScalarFieldOf BLS12_381_G1_Point)) omega gs

validationGroupElement
  :: forall n m
   . (KnownNat n, KnownNat m, KnownNat (PlonkupPolyExtendedLength m))
  => UtxoAccumulatorCRS
  -> BLS12_381_G1_Point
validationGroupElement crs = utxoAccumulatorGroupElements @n @m crs !! 0

accumulationGroupElements
  :: forall n m
   . (KnownNat n, KnownNat m, KnownNat (PlonkupPolyExtendedLength m))
  => UtxoAccumulatorCRS
  -> Vector n BLS12_381_G1_Point
accumulationGroupElements crs =
  tabulate
    ( \(toConstant -> i) ->
        utxoAccumulatorGroupElements @n @m crs !! (i + 1)
    )

distributionGroupElements
  :: forall n m
   . (KnownNat n, KnownNat m, KnownNat (PlonkupPolyExtendedLength m))
  => UtxoAccumulatorCRS
  -> Vector n BLS12_381_G1_Point
distributionGroupElements crs = tabulate (\(toConstant -> i) -> utxoAccumulatorGroupElements @n @m crs !! (value @n + i + 1))
