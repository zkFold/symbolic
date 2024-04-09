{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Tests.ByteString (specByteString) where

import Control.Applicative ((<*>))
import Control.Monad (return)
import Data.Data (Proxy (..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (map, (++))
import GHC.TypeNats (KnownNat, natVal)
import Numeric.Natural (Natural)
import System.IO (IO)
import Test.Hspec (describe, hspec)
import Test.QuickCheck (Gen, Property, (===))
import Tests.ArithmeticCircuit (eval', it)
import ZkFold.Base.Algebra.Basic.Class
import ZkFold.Base.Algebra.Basic.Field (Zp)
import ZkFold.Base.Algebra.Basic.Number (Prime)
import ZkFold.Prelude (chooseNatural)
import ZkFold.Symbolic.Compiler (ArithmeticCircuit)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.ByteString
import Prelude (show)
import qualified Prelude as Haskell

toss :: Natural -> Gen Natural
toss x = chooseNatural (0, x)

value :: forall a n. ByteString n (ArithmeticCircuit a) -> ByteString n a
value (ByteString x xs) = ByteString (eval' x) (map eval' xs)

type Binary a = a -> a -> a

type UBinary n a = Binary (ByteString n a)

isHom :: (KnownNat n, Prime p) => UBinary n (Zp p) -> UBinary n (ArithmeticCircuit (Zp p)) -> Natural -> Natural -> Property
isHom f g x y = value (fromConstant x `g` fromConstant y) === fromConstant x `f` fromConstant y

isRightNeutral ::
  (KnownNat n, Prime p) =>
  UBinary n (Zp p) ->
  UBinary n (ArithmeticCircuit (Zp p)) ->
  ByteString n (Zp p) ->
  ByteString n (ArithmeticCircuit (Zp p)) ->
  Natural ->
  Property
isRightNeutral f g n1 n2 x = value (fromConstant x `g` n2) === fromConstant x `f` n1

isLeftNeutral ::
  (KnownNat n, Prime p) =>
  UBinary n (Zp p) ->
  UBinary n (ArithmeticCircuit (Zp p)) ->
  ByteString n (Zp p) ->
  ByteString n (ArithmeticCircuit (Zp p)) ->
  Natural ->
  Property
isLeftNeutral f g n1 n2 x = value (n2 `g` fromConstant x) === n1 `f` fromConstant x

specByteString :: forall p n. (Prime p, KnownNat n) => IO ()
specByteString = hspec $ do
  let n = Haskell.toInteger $ natVal (Proxy :: Proxy n)
      m = 2 ^ n - 1
  describe ("ByteString" ++ show n ++ " specification") $ do
    it "Zp embeds Integer" $ do
      x <- toss m
      return $ toConstant @(ByteString n (Zp p)) @Natural (fromConstant x) === x
    it "Integer embeds Zp" $ \(x :: ByteString n (Zp p)) ->
      fromConstant (toConstant @_ @Natural x) === x
    it "AC embeds Integer" $ do
      x <- toss m
      return $ value @(Zp p) @n (fromConstant x) === fromConstant x
    it "applies bitwise OR correctly" $ isHom @n @p (||) (||) <$> toss m <*> toss m
    it "has false" $ value @(Zp p) @n false === false
    it "obeys left additive neutrality" $ isLeftNeutral @n @p (||) (||) false false <$> toss m
    it "obeys right additive neutrality" $ isRightNeutral @n @p (||) (||) false false <$> toss m
    it "applies bitwise not correctly" $ do
      x <- toss m
      return $ value @(Zp p) @n (not (fromConstant x)) === not (fromConstant x)
    it "Applies bitwise AND correctly" $ isHom @n @p (&&) (&&) <$> toss m <*> toss m
    it "has true" $ value @(Zp p) @n true === true
    it "obeys left multiplicative neutrality" $ isLeftNeutral @n @p (&&) (&&) true true <$> toss m
    it "obeys right multiplicative neutrality" $ isRightNeutral @n @p (&&) (&&) true true <$> toss m
