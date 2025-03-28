{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}

module Tests.Symbolic.Compiler.Test2 (specArithmetization2) where

import           Data.Binary                                 (Binary)
import           GHC.Generics                                (Par1 (..), U1 (..), (:*:) (..))
import           Prelude                                     hiding (Bool, Eq (..), Num (..), not, replicate, (/), (^),
                                                              (||))
import qualified Prelude                                     as Haskell
import           Test.Hspec
import           Test.QuickCheck                             (property)

import           ZkFold.Base.Algebra.Basic.Class             (one)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Symbolic.Class                       (Arithmetic, Symbolic)
import           ZkFold.Symbolic.Compiler
import           ZkFold.Symbolic.Data.Bool                   (Bool (..), BoolType (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Data.FieldElement           (FieldElement)

-- A true statement.
tautology :: Symbolic c => FieldElement c -> FieldElement c -> Bool c
tautology x y = (x /= y) || (x == y)

testTautology :: forall a . (Arithmetic a, Binary a) => a -> a -> Haskell.Bool
testTautology x y =
    let Bool ac = compile @a tautology
        b       = unPar1 (eval ac ((U1 :*: U1 :*: U1) :*: Par1 x :*: Par1 y :*: U1))
    in b Haskell.== one

specArithmetization2 :: Spec
specArithmetization2 = do
    describe "Arithmetization test 2" $ do
        it "should pass" $ property $ \x y -> testTautology @Fr x y
