{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as B16
import Data.Eq (Eq)
import Data.Function (const, ($))
import Data.Functor (Functor)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics ((:*:) (..))
import System.IO (IO)
import Test.Hspec (describe, hspec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Text.Show (Show)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Base)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Data.Orphans ()
import ZkFold.Symbolic.Compiler (compile)
import ZkFold.Symbolic.Compiler.ArithmeticCircuit (eval)
import ZkFold.Symbolic.Compiler.ArithmeticCircuit.Context (CircuitContext)
import ZkFold.Symbolic.Data.Bool (Bool, false, true)
import ZkFold.Symbolic.Data.Class (SymbolicData (..), SymbolicFunction (..), SymbolicFunctionResult, symFunc1)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import Prelude (either, error, id, (.), type (~))

import ZkFold.Symbolic.UPLC.Class (Sym)
import ZkFold.Symbolic.UPLC.Converter (ScriptCtx, contractV3)
import ZkFold.UPLC.BuiltinFunction
import ZkFold.UPLC.Term

areSame
  :: ( SymbolicFunction f
     , ContextF f ~ c
     , Support f ~ s
     , Domain f ~ l
     , c ~ CircuitContext a
     , Arbitrary (i a)
     , Show (i a)
     , SymbolicInput s
     , Context s ~ c
     , i ~ Payload s :*: Layout s
     , Functor l
     , Eq (l a)
     , Show (l a)
     , a ~ Zp BLS12_381_Base
     )
  => (Term -> f)
  -> Term
  -> f
  -> Property
areSame v t f =
  let acT = compile (v t)
      acF = compile f
   in property $ \i -> eval acT i === eval acF i

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

symContractV3 :: Sym c => Term -> ScriptCtx c -> SymbolicFunctionResult (Bool c)
symContractV3 t = symFunc1 $ contractV3 t

main :: IO ()
main = hspec $ describe "UPLC tests" $ do
  prop "false is ok" $ areSame symContractV3 (TLam tFalse) (symFunc1 $ const true)
  prop "error is not ok" $ areSame symContractV3 (TLam TError) (symFunc1 $ const false)
  prop "substitution is ok" $
    areSame symContractV3 (TLam $ TLam (TVariable 0) $$ tTrue) (symFunc1 $ const true)
  prop "pair is ok" $
    areSame
      symContractV3
      (TLam $ TBuiltin (BFPoly FstPair) $$ TConstant (CPair (CUnit ()) (CBool false)))
      (symFunc1 $ const true)
  prop "bool is not a pair" $
    areSame symContractV3 (TLam $ TBuiltin (BFPoly SndPair) $$ tTrue) (symFunc1 $ const false)
  prop "trivial if if ok" $
    areSame
      symContractV3
      (TLam $ TBuiltin (BFPoly IfThenElse) $$ tTrue $$ tTrue $$ tFalse)
      (symFunc1 $ const true)
  prop "lazy error in if is ok" $
    areSame
      symContractV3
      (TLam $ TBuiltin (BFPoly IfThenElse) $$ tTrue $$ tUnit $$ TError)
      (symFunc1 $ const true)
  prop "error propagation in if" $
    areSame
      symContractV3
      (TLam $ TBuiltin (BFPoly IfThenElse) $$ tFalse $$ tUnit $$ TError)
      (symFunc1 $ const false)
  prop "if as an argument is ok" $
    areSame
      symContractV3
      (TLam $ TLam (TVariable 0 $$ tTrue $$ tUnit $$ TError) $$ TBuiltin (BFPoly IfThenElse))
      (symFunc1 $ const true)
  prop "strings are equal" $
    areSame
      symContractV3
      ( TLam $
          TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
            $$ (TBuiltin (BFMono $ BMFString EqualsString) $$ tString1 $$ tString1)
      )
      (symFunc1 $ const true)
  prop "strings are not equal" $
    areSame
      symContractV3
      ( TLam $
          TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
            $$ (TBuiltin (BFMono $ BMFString EqualsString) $$ tString1 $$ tString2)
      )
      (symFunc1 $ const false)
  prop "append string is correct" $
    areSame
      symContractV3
      ( TLam $
          TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
            $$ ( TBuiltin (BFMono $ BMFString EqualsString)
                   $$ tString12
                   $$ (TBuiltin (BFMono $ BMFString AppendString) $$ tString1 $$ tString2)
               )
      )
      (symFunc1 $ const true)
  prop "append string is correct-2" $
    areSame
      symContractV3
      ( TLam $
          TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
            $$ ( TBuiltin (BFMono $ BMFString EqualsString)
                   $$ tString12
                   $$ (TBuiltin (BFMono $ BMFString AppendString) $$ tString2 $$ tString1)
               )
      )
      (symFunc1 $ const false)
  -- Hash result obtained from https://emn178.github.io/online-tools/sha256.html.
  prop "sha2_256 on empty string is correct" $
    areSame
      symContractV3
      ( TLam $
          TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
            $$ ( TBuiltin (BFMono $ BMFByteString EqualsByteString)
                   $$ unsafeTBSFromHex "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                   $$ (TBuiltin (BFMono $ BMFAlgorithm SHA2_256) $$ tBSFromUtf8 "")
               )
      )
      (symFunc1 $ const true)
  prop "sha2_256 on small (length < 256) string is correct" $
    areSame
      symContractV3
      ( TLam $
          TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
            $$ ( TBuiltin (BFMono $ BMFByteString EqualsByteString)
                   $$ unsafeTBSFromHex "fb8e20fc2e4c3f248c60c39bd652f3c1347298bb977b8b4d5903b85055620603"
                   $$ (TBuiltin (BFMono $ BMFAlgorithm SHA2_256) $$ tBSFromUtf8 "ab")
               )
      )
      (symFunc1 $ const true)
  prop "sha2_256 on large (length > 256) string is correct" $
    areSame
      symContractV3
      ( TLam $
          TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
            $$ ( TBuiltin (BFMono $ BMFByteString EqualsByteString)
                   $$ unsafeTBSFromHex "ac137fce49837c7c2945f6160d3c0e679e6f40070850420a22bc10e0692cbdc7"
                   $$ ( TBuiltin (BFMono $ BMFAlgorithm SHA2_256)
                          $$ tBSFromUtf8 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                      )
               )
      )
      (symFunc1 $ const true)
