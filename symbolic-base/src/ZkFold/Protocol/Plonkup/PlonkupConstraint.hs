module ZkFold.Protocol.Plonkup.PlonkupConstraint where

import           Data.Ord                                       (Ord)

import           ZkFold.Algebra.Class
import           ZkFold.Protocol.Plonkup.LookupConstraint       (LookupConstraint (..))
import           ZkFold.Protocol.Plonkup.PlonkConstraint        (PlonkConstraint (..), toPlonkConstraint)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var (Var)

data PlonkupConstraint i a = ConsPlonk (PlonkConstraint i a) | ConsLookup (LookupConstraint i a) | ConsExtra

getPlonkConstraint :: (Ord a, FiniteField a) => PlonkupConstraint i a -> PlonkConstraint i a
getPlonkConstraint (ConsPlonk c) = c
getPlonkConstraint _             = toPlonkConstraint zero

isLookupConstraint :: FiniteField a => PlonkupConstraint i a -> a
isLookupConstraint (ConsLookup _) = one
isLookupConstraint _              = zero

getA :: forall a i . (Ord a, FiniteField a) => PlonkupConstraint i a -> Var a
getA (ConsPlonk c)  = x1 c
getA (ConsLookup c) = lkVar1 c
getA ConsExtra      = x1 (toPlonkConstraint zero)

getB :: forall a i . (Ord a, FiniteField a) => PlonkupConstraint i a -> Var a
getB (ConsPlonk c)  = x2 c
getB (ConsLookup c) = lkVar2 c
getB ConsExtra      = x2 (toPlonkConstraint zero)

getC :: forall a i . (Ord a, FiniteField a) => PlonkupConstraint i a -> Var a
getC (ConsPlonk c)  = x3 c
getC (ConsLookup c) = lkVar3 c
getC ConsExtra      = x3 (toPlonkConstraint zero)
