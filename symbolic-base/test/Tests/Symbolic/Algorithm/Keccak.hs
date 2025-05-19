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
      describe "padding" $ do
        it "padding result for empty string" $
          toConstant (padding @"Keccak256" @(Interpreter Element) @0 (fromConstant ("" :: Bytes.ByteString))) `shouldBe` 12953744211667879574559702190010707651171625584905095574841686913755541732351702201085464556891975967691972983316275962328947759192690782519488857596207264634977115153700247127325882095493965845839345131422255684387968478939180332711558614761546211605811843616790961251349338535374752162059924960553344400851693295429983666304
        it "padding result for single character" $
          toConstant (padding @"Keccak256" @(Interpreter Element) @8 (fromConstant ("a" :: Bytes.ByteString))) `shouldBe` 1256563789095111146386879236267718371490410070898235306289232855973794405623007112342013052114447028147245176151144847548885780093875352352759635940182089854220260549264993112940951679989850210350501787689878419571900082958971348446195840157708894815652834033342851331073273921285004767345445767755551568384180076583643688992896