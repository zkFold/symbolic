{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -Wno-orphans #-}
import           Control.Applicative                         ((<*>))
import           Control.Monad                               (return)
import           Data.Eq                                     (Eq)
import           Data.Function                               (const, ($))
import           Data.Functor                                (Functor, (<$>))
import           Data.Functor.Rep                            (Rep, Representable)
import qualified Data.Text                                   as T
import           GHC.Generics                                (Par1 (..), U1 (..), (:*:) (..))
import           Prelude                                     (type (~), (.))
import           System.IO                                   (IO)
import           Test.Hspec                                  (describe, hspec)
import           Test.Hspec.QuickCheck                       (prop)
import           Test.QuickCheck
import           Text.Show                                   (Show)

import           ZkFold.Base.Algebra.Basic.Field             (Zp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Base)
import           ZkFold.Base.Data.ByteString                 (Binary)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit, compile)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit  (eval)
import           ZkFold.Symbolic.Data.Bool                   (false, true)
import           ZkFold.Symbolic.Data.Class                  (SymbolicData (..))
import           ZkFold.Symbolic.Data.Input                  (SymbolicInput)
import           ZkFold.Symbolic.UPLC.Converter              (contractV3)
import           ZkFold.UPLC.BuiltinFunction
import           ZkFold.UPLC.Term

areSame ::
  ( SymbolicData f, Context f ~ c, Support f ~ s, Layout f ~ l
  , c ~ ArithmeticCircuit a p i, Arbitrary (i a), Show (i a)
  , Representable p, Binary (Rep p), Arbitrary (p a), Show (p a)
  , SymbolicInput s, Context s ~ c, Layout s ~ i, Payload s ~ p
  , Functor l, Eq (l a), Show (l a)
  , a ~ Zp BLS12_381_Base) =>
  (Term -> f) -> Term -> f -> Property
areSame v t f =
  let acT = compile (v t)
      acF = compile f
   in property $ \p i -> eval acT p i === eval acF p i

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary ((f :*: g) a) where
  arbitrary = (:*:) <$> arbitrary <*> arbitrary

instance Arbitrary (U1 a) where
  arbitrary = return U1

instance Arbitrary a => Arbitrary (Par1 a) where
  arbitrary = Par1 <$> arbitrary

tFalse, tTrue, tUnit :: Term
tFalse = TConstant (CBool false)
tTrue = TConstant (CBool true)
tUnit = TConstant (CUnit ())

tString1, tString2, tString12 :: Term
tString1 = TConstant (CString . T.pack $ "1")
tString2 = TConstant (CString . T.pack $ "2")
tString12 = TConstant (CString . T.pack $ "12")

infixl 1 $$
($$) :: Term -> Term -> Term
($$) = TApp

main :: IO ()
main = hspec $ describe "UPLC tests" $ do
  prop "false is ok" $ areSame contractV3 (TLam tFalse) (const true)
  prop "error is not ok" $ areSame contractV3 (TLam TError) (const false)
  prop "substitution is ok" $
    areSame contractV3 (TLam $ TLam (TVariable 0) $$ tTrue) (const true)
  prop "pair is ok" $ areSame contractV3
    (TLam $ TBuiltin (BFPoly FstPair) $$ TConstant (CPair (CUnit ()) (CBool false)))
    (const true)
  prop "bool is not a pair" $
    areSame contractV3 (TLam $ TBuiltin (BFPoly SndPair) $$ tTrue) (const false)
  prop "trivial if if ok" $ areSame contractV3
    (TLam $ TBuiltin (BFPoly IfThenElse) $$ tTrue $$ tTrue $$ tFalse)
    (const true)
  prop "lazy error in if is ok" $ areSame contractV3
    (TLam $ TBuiltin (BFPoly IfThenElse) $$ tTrue $$ tUnit $$ TError)
    (const true)
  prop "error propagation in if" $ areSame contractV3
    (TLam $ TBuiltin (BFPoly IfThenElse) $$ tFalse $$ tUnit $$ TError)
    (const false)
  prop "if as an argument is ok" $ areSame contractV3
    (TLam $ TLam (TVariable 0 $$ tTrue $$ tUnit $$ TError) $$ TBuiltin (BFPoly IfThenElse))
    (const true)
  prop "strings are equal" $ areSame contractV3
    (TLam $ TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
                                               $$ (TBuiltin (BFMono $ BMFString EqualsString) $$ tString1 $$ tString1))
    (const true)
  prop "strings are not equal" $ areSame contractV3
    (TLam $ TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
                                               $$ (TBuiltin (BFMono $ BMFString EqualsString) $$ tString1 $$ tString2))
    (const false)
  prop "append string is correct" $ areSame contractV3
    (TLam $ TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
                                               $$ (TBuiltin (BFMono $ BMFString EqualsString) $$ tString12
                                                                                              $$ (TBuiltin (BFMono $ BMFString AppendString) $$ tString1 $$ tString2)))
    (const true)
  prop "append string is correct-2" $ areSame contractV3
    (TLam $ TLam (TBuiltin (BFPoly IfThenElse) $$ TVariable 0 $$ tUnit $$ TError)
                                               $$ (TBuiltin (BFMono $ BMFString EqualsString) $$ tString12
                                                                                              $$ (TBuiltin (BFMono $ BMFString AppendString) $$ tString2 $$ tString1)))
    (const false)
