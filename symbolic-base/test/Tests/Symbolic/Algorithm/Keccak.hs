{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Tests.Symbolic.Algorithm.Keccak (specKeccak) where

import           Control.Monad                          (forM_)
import           Data.Binary                            (Binary)
import           Data.Bits                              (shiftR)
import qualified Data.ByteString                   as Bytes
import           Data.Function                          (($))
import           Data.Functor                           ((<$>))
import           Data.List                              (isPrefixOf, isSuffixOf, take, (++))
import           Data.List.Split                        (splitOn)
import           Data.Proxy                             (Proxy (..))
import           GHC.Generics                           (U1)
import           GHC.TypeLits                           (KnownSymbol, Symbol, symbolVal)
import           Prelude                                (String, otherwise, pure, read, (<>), (==))
import qualified Prelude                                as Haskell
import           System.Directory                       (listDirectory)
import           System.Environment                     (lookupEnv)
import           System.FilePath.Posix
import           System.IO                              (IO)
import           Test.Hspec                             (Spec, describe, runIO, shouldBe)
import           Test.QuickCheck                        (Gen, withMaxSuccess, (===))
import           Tests.Symbolic.ArithmeticCircuit       (it)
import           Text.Regex.TDFA

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import           ZkFold.Algebra.Field                   (Zp)
import           ZkFold.Algebra.Number
import           ZkFold.Data.Vector                     (Vector)
import           ZkFold.Prelude                         (chooseNatural)
import           ZkFold.Symbolic.Algorithm.Hash.SHA2
import           ZkFold.Symbolic.Class                  (Arithmetic)
import           ZkFold.Symbolic.Compiler               (ArithmeticCircuit, exec)
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.VarByteString     (fromNatural)
import           ZkFold.Symbolic.Interpreter            (Interpreter (Interpreter))
import ZkFold.Symbolic.Algorithm.Hash.Keccak (padding)

type Element = Zp BLS12_381_Scalar

specKeccak :: Spec
specKeccak = do
    describe "Keccak" $
      describe "padding" $
        it "padding result for 0" $ do
          let res = (padding @"Keccak256" @(Interpreter Element) @0 (fromConstant ("" :: Bytes.ByteString))) 
          Haskell.putStr $ Haskell.show res
          toConstant res `shouldBe` 12953744211667879574559702190010707651171625584905095574841686913755541732351702201085464556891975967691972983316275962328947759192690782519488857596207264634977115153700247127325882095493965845839345131422255684387968478939180332711558614761546211605811843616790961251349338535374752162059924960553344400851693295429983666304