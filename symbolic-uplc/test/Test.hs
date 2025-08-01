{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (return)
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as B16
import Data.Eq (Eq)
import Data.Function (const, ($))
import Data.Functor.Rep (tabulate)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics ((:*:) (..))
import System.IO (IO)
import Test.Hspec (describe, hspec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, Property, property, (===))
import Test.QuickCheck.Instances ()
import Text.Show (Show)
import ZkFold.Algebra.Class (zero)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.ArithmeticCircuit (eval)
import ZkFold.ArithmeticCircuit.Context (CircuitContext)
import ZkFold.Data.Orphans ()
import ZkFold.Symbolic.Compiler (compile)
import ZkFold.Symbolic.Data.Bool (false, true)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import Prelude (either, error, id, (.), type (~))

import ZkFold.Symbolic.UPLC.Converter (contractV3)
import ZkFold.UPLC.BuiltinFunction
import ZkFold.UPLC.Constant
import ZkFold.UPLC.Term

areSame
  :: forall a x y
   . ( a ~ Zp BLS12_381_Scalar
     , SymbolicInput x
     , SymbolicData y
     , Context x ~ CircuitContext a
     , Context y ~ CircuitContext a
     , Eq (Layout y a)
     , Arbitrary (Layout x a)
     , Show (Layout y a)
     , Show (Layout x a)
     )
  => (Term -> x -> y)
  -> Term
  -> (x -> y)
  -> Property
areSame v t f =
  let acT = compile (v t)
      acF = compile f
      p = tabulate (const zero)
   in property $ \i -> eval acT (p :*: i) === eval acF (p :*: i)

tFalse, tTrue, tUnit :: Term
tFalse = TConstant (CBool false)
tTrue = TConstant (CBool true)
tUnit = TConstant (CUnit ())

tString1, tString2, tString12 :: Term
tString1 = TConstant (CString . T.pack $ "1")
tString2 = TConstant (CString . T.pack $ "2")
tString12 = TConstant (CString . T.pack $ "12")

unsafeTBSFromHex :: B.ByteString -> Term
unsafeTBSFromHex bs = TConstant (CByteString $ either error id $ B16.decode bs)

tBSFromUtf8 :: T.Text -> Term
tBSFromUtf8 = TConstant . CByteString . TE.encodeUtf8

infixl 1 $$

($$) :: Term -> Term -> Term
($$) = TApp

-- type C = Interpreter (Zp BLS12_381_Scalar)

-- toProp :: Symbolic.Bool C -> Property
-- toProp (Symbolic.Bool (Interpreter (Par1 x))) = x === one

main :: IO ()
main = hspec do
  describe "Data tests" do
    return ()
  -- FIXME These tests do not pass yet
  -- prop "fold/unfold" \(fromConstant @Data @(Symbolic.Data C) -> d) ->
  --  toProp (Symbolic.unfoldData d Symbolic.foldData == d)
  -- prop "unfold/fold" \(fromConstant @Data
  --                    @(Symbolic.DataCell (Symbolic.Data C) C) -> d) ->
  --  toProp (Symbolic.unfoldData (Symbolic.foldData d) inject == inject d)
  describe "UPLC tests" do
    prop "false is ok" $ areSame contractV3 (TLam tFalse) (const true)
    prop "error is not ok" $ areSame contractV3 (TLam TError) (const false)
    prop "substitution is ok" $
      areSame contractV3 (TLam $ TLam (TVariable 0) $$ tTrue) (const true)
    prop "pair is ok" $
      areSame
        contractV3
        (TLam $ TBuiltin (BFPoly FstPair) $$ TConstant (CPair (CUnit ()) (CBool false)))
        (const true)
    prop "bool is not a pair" $
      areSame contractV3 (TLam $ TBuiltin (BFPoly SndPair) $$ tTrue) (const false)
    prop "trivial if if ok" $
      areSame
        contractV3
        (TLam $ TBuiltin (BFPoly IfThenElse) $$ tTrue $$ tTrue $$ tFalse)
        (const true)
    prop "lazy error in if is ok" $
      areSame
        contractV3
        (TLam $ TBuiltin (BFPoly IfThenElse) $$ tTrue $$ tUnit $$ TError)
        (const true)
    prop "error propagation in if" $
      areSame
        contractV3
        (TLam $ TBuiltin (BFPoly IfThenElse) $$ tFalse $$ tUnit $$ TError)
        (const false)
    prop "if as an argument is ok" $
      areSame
        contractV3
        (TLam $ TLam (TVariable 0 $$ tTrue $$ tUnit $$ TError) $$ TBuiltin (BFPoly IfThenElse))
        (const true)
    prop "strings are equal" $
      areSame
        contractV3
        ( TLam $
            TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
              $$ (TBuiltin (BFMono $ BMFString EqualsString) $$ tString1 $$ tString1)
        )
        (const true)
    prop "strings are not equal" $
      areSame
        contractV3
        ( TLam $
            TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
              $$ (TBuiltin (BFMono $ BMFString EqualsString) $$ tString1 $$ tString2)
        )
        (const false)
    prop "append string is correct" $
      areSame
        contractV3
        ( TLam $
            TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
              $$ ( TBuiltin (BFMono $ BMFString EqualsString)
                     $$ tString12
                     $$ (TBuiltin (BFMono $ BMFString AppendString) $$ tString1 $$ tString2)
                 )
        )
        (const true)
    prop "append string is correct-2" $
      areSame
        contractV3
        ( TLam $
            TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
              $$ ( TBuiltin (BFMono $ BMFString EqualsString)
                     $$ tString12
                     $$ (TBuiltin (BFMono $ BMFString AppendString) $$ tString2 $$ tString1)
                 )
        )
        (const false)
    -- Hash result obtained from https://emn178.github.io/online-tools/sha256.html.
    prop "sha2_256 on empty string is correct" $
      areSame
        contractV3
        ( TLam $
            TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
              $$ ( TBuiltin (BFMono $ BMFByteString EqualsByteString)
                     $$ unsafeTBSFromHex "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                     $$ (TBuiltin (BFMono $ BMFAlgorithm SHA2_256) $$ tBSFromUtf8 "")
                 )
        )
        (const true)
    prop "sha2_256 on small (length < 256) string is correct" $
      areSame
        contractV3
        ( TLam $
            TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
              $$ ( TBuiltin (BFMono $ BMFByteString EqualsByteString)
                     $$ unsafeTBSFromHex "fb8e20fc2e4c3f248c60c39bd652f3c1347298bb977b8b4d5903b85055620603"
                     $$ (TBuiltin (BFMono $ BMFAlgorithm SHA2_256) $$ tBSFromUtf8 "ab")
                 )
        )
        (const true)
    prop "sha2_256 on large (length > 256) string is correct" $
      areSame
        contractV3
        ( TLam $
            TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
              $$ ( TBuiltin (BFMono $ BMFByteString EqualsByteString)
                     $$ unsafeTBSFromHex "ac137fce49837c7c2945f6160d3c0e679e6f40070850420a22bc10e0692cbdc7"
                     $$ ( TBuiltin (BFMono $ BMFAlgorithm SHA2_256)
                            $$ tBSFromUtf8 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                        )
                 )
        )
        (const true)
