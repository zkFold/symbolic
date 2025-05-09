{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}

module ZkFold.Symbolic.Examples.UtxoAccumulator where

import           Data.Function                       (const, ($))
import           Data.Functor                        (fmap)
import           Data.Functor.Rep                    (tabulate)
import           GHC.Generics                        (Par1 (..), U1 (..), (:*:) (..), (:.:) (..))

import           ZkFold.Algebra.Number               (KnownNat)
import           ZkFold.Data.ByteString              (Binary)
import           ZkFold.Data.HFunctor                (hmap)
import           ZkFold.Data.Vector                  (Vector)
import           ZkFold.Symbolic.Algorithm.Hash.MiMC (hash)
import           ZkFold.Symbolic.Class               (Arithmetic, Symbolic)
import           ZkFold.Symbolic.Compiler            (ArithmeticCircuit, compile, hlmap)
import           ZkFold.Symbolic.Data.Bool           (Bool (..), BoolType (..), all, any)
import           ZkFold.Symbolic.Data.Eq             (Eq (..))
import           ZkFold.Symbolic.Data.FieldElement   (FieldElement)

utxoAccumulator :: forall n c . Symbolic c
    => Vector n (FieldElement c)
    -> Vector n (FieldElement c)
    -> (FieldElement c, FieldElement c, FieldElement c)
    -> (Vector n (FieldElement c), Bool c, Vector n (FieldElement c))
utxoAccumulator hs cs (a, c, r) =
    let
        h = hash (a, hash (c, r))

        cond1 = any (== h) hs
        cond2 = all (/= c) cs
    in
        (hs, cond1 && cond2, cs)

type UtxoAccumulatorInput n = Vector n :*: Vector n :*: Par1 :*: Par1 :*: Par1
type UtxoAccumulatorOutput n = Vector n :*: Par1 :*: Vector n

utxoAccumulatorCircuit :: forall n a . (KnownNat n, Arithmetic a, Binary a)
    => ArithmeticCircuit a (UtxoAccumulatorInput n) (UtxoAccumulatorOutput n)
utxoAccumulatorCircuit =
    hmap (\(Comp1 i1 :*: i2 :*: Comp1 i3) -> fmap unPar1 i1 :*: i2 :*: fmap unPar1 i3) $
    hlmap (\(i1 :*: i2 :*: i3) -> (Comp1 (tabulate $ const U1) :*: (Comp1 (tabulate $ const U1) :*: ((U1 :*: (U1 :*: U1)) :*: U1)))
        :*: ((Comp1 $ fmap Par1 i1) :*: ((Comp1 $ fmap Par1 i2) :*: (i3 :*: U1))))
    $ compile @a $ utxoAccumulator @n

utxoAccumulatorInput :: forall n a .
       Vector n a
    -> Vector n a
    -> (a, a, a)
    -> UtxoAccumulatorInput n a
utxoAccumulatorInput hs cs (a, c, r) =
    hs :*: cs :*: (Par1 a :*: (Par1 c :*: Par1 r))
