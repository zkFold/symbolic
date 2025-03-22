{-# LANGUAGE TypeOperators #-}


module ZkFold.Symbolic.Data.Lookup where


import           Data.Functor.Rep
import qualified Data.Set                                          as S
import           Data.Traversable                                  (Traversable)
import           Data.Typeable                                     (Typeable)
import           GHC.Generics                                      (Par1 (Par1), (:*:) ((:*:)))
import           Prelude                                           (fmap, return, ($))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Data.ByteString                       (Binary)
import           ZkFold.Symbolic.Class                             (Arithmetic)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Lookup
import           ZkFold.Symbolic.MonadCircuit
import ZkFold.Base.Algebra.Basic.Number (Natural)


binLookup :: Arithmetic a => LookupTable a Par1
binLookup = Ranges $ S.singleton (zero, one)

bin2Lookup :: Arithmetic a => LookupTable a (Par1 :*: Par1)
bin2Lookup = Product binLookup binLookup

newBinLookup :: forall f var a w m.
  ( Traversable f, Typeable f, Representable f
  , MonadCircuit var a w m, Binary (Rep f))
   => LookupTable a f -> f var -> (forall x. ResidueField x => f x -> Par1 x) -> m (Par1 var)
newBinLookup dom vars f = do
    let vs = fmap (at @var @w) vars
        (Par1 v3w) = f vs
    v3 <- unconstrained v3w
    fId <- registerFunction f
    lookupConstraint (vars :*: (Par1 v3)) (Plot fId dom)
    return $ Par1 v3

powBinLookup :: Arithmetic a => Natural -> LookupTable a Par1
powBinLookup n = Ranges $ S.singleton (zero, (one + one) ^ n)

powBin2Lookup :: Arithmetic a => Natural -> LookupTable a (Par1 :*: Par1)
powBin2Lookup n = Product (powBinLookup n) (powBinLookup n)


--------------------------------------------------------------------------------

orOp :: (AdditiveGroup p, MultiplicativeSemigroup p) => (:*:) Par1 Par1 p -> Par1 p
orOp ((Par1 v1) :*: (Par1 v2)) = Par1 (v1 + v2 - v1 * v2)

andOp :: MultiplicativeSemigroup p => (:*:) Par1 Par1 p -> Par1 p
andOp ((Par1 v1) :*: (Par1 v2)) = Par1 (v1 * v2)

xorOp :: (AdditiveGroup p, MultiplicativeMonoid p) => (:*:) Par1 Par1 p -> Par1 p
xorOp ((Par1 v1) :*: (Par1 v2)) = Par1 (v1 + v2 - (one + one) * v1 * v2)
