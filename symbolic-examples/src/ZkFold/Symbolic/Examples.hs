{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples (ExampleOutput (..), examples) where

import           Control.DeepSeq                             (NFData, NFData1)
import           Data.Function                               (const, ($), (.))
import           Data.Functor.Rep                            (Rep, Representable)
import           Data.String                                 (String)
import           Data.Type.Equality                          (type (~))
import           Examples.Blake2b                            (exampleBlake2b_224, exampleBlake2b_256)
import           Examples.ByteString
import           Examples.Conditional                        (exampleConditional)
import           Examples.Constant                           (exampleConst5, exampleEq5)
import           Examples.Eq                                 (exampleEq)
import           Examples.FFA
import           Examples.Fibonacci                          (exampleFibonacci)
import           Examples.LEQ                                (exampleLEQ)
import           Examples.MiMCHash                           (exampleMiMC)
import           Examples.ReverseList                        (exampleReverseList)
import           Examples.UInt

import           ZkFold.Base.Algebra.Basic.Field             (Zp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit, compile)
import           ZkFold.Symbolic.Data.ByteString             (ByteString)
import           ZkFold.Symbolic.Data.Class                  (SymbolicData (..))
import           ZkFold.Symbolic.Data.Combinators            (RegisterSize (Auto))
import           ZkFold.Symbolic.Data.Input                  (SymbolicInput)

type A = Zp BLS12_381_Scalar
type C a = ArithmeticCircuit a

data ExampleOutput where
  ExampleOutput ::
    forall a p i o.
    (Representable p, Representable i, NFData (Rep i), NFData1 o) =>
    (() -> C a p i o) -> ExampleOutput

exampleOutputBLS ::
  forall p i o c f.
  ( SymbolicData f
  , c ~ C A p i
  , Context f ~ c
  , Layout f ~ o
  , SymbolicInput (Support f)
  , Context (Support f) ~ c
  , Layout (Support f) ~ i
  , Payload (Support f) ~ p
  , Representable i
  , NFData (Rep i)
  , NFData1 o
  ) => f -> ExampleOutput
exampleOutputBLS = ExampleOutput @A @p @i @o . const . compile

examples :: [(String, ExampleOutput)]
examples =
  [ ("Constant.5", exampleOutputBLS exampleConst5)
  , ("Eq.Constant.5", exampleOutputBLS exampleEq5)
  , ("Eq", exampleOutputBLS exampleEq)
  , ("Conditional", exampleOutputBLS exampleConditional)
  , ("LEQ", exampleOutputBLS exampleLEQ)
  , ("ByteString.And.32", exampleOutputBLS $ exampleByteStringAnd @32)
  , ("ByteString.Or.64", exampleOutputBLS $ exampleByteStringOr @64)
  , ("ByteString.Extend.1.512", exampleOutputBLS $ exampleByteStringResize @1 @512)
  , ("ByteString.Truncate.512.1", exampleOutputBLS $ exampleByteStringResize @512 @1)
  , ("ByteString.Truncate.74.54", exampleOutputBLS $ exampleByteStringResize @74 @54)
  , ("ByteString.Add.512", exampleOutputBLS $ exampleByteStringAdd @512)
  , ("UInt.Extend.1.512", exampleOutputBLS $ exampleUIntResize @1 @512 @Auto)
  , ("UInt.Truncate.512.1", exampleOutputBLS $ exampleUIntResize @512 @1 @Auto)
  , ("UInt.Truncate.74.54", exampleOutputBLS $ exampleUIntResize @74 @54 @Auto)
  , ("UInt.StrictAdd.256.Auto", exampleOutputBLS $ exampleUIntStrictAdd @256 @Auto)
  , ("UInt.StrictMul.512.Auto", exampleOutputBLS $ exampleUIntStrictMul @512 @Auto)
  , ("UInt.Mul.64.Auto", exampleOutputBLS $ exampleUIntMul @64 @Auto)
  , ("UInt.Mul.4096.Auto", exampleOutputBLS $ exampleUIntMul @4096 @Auto)
  , ("UInt.LEQ.256.Auto", exampleOutputBLS $ exampleUIntLeq @256 @Auto)
  , ("UInt.ProductMod.1024.Auto", exampleOutputBLS $ exampleUIntProductMod @1024 @Auto)
  , ("UInt.DivMod.32.Auto", exampleOutputBLS $ exampleUIntDivMod @32 @Auto)
  , ("UInt.ExpMod.32.16.64.Auto", exampleOutputBLS $ exampleUIntExpMod @32 @16 @64 @Auto)
  , ("UInt.ExpMod.256.64.1024.Auto", exampleOutputBLS $ exampleUIntExpMod @256 @64 @1024 @Auto)
  , ("FFA.Add.337", exampleOutputBLS exampleFFAadd337)
  , ("FFA.Add.097", exampleOutputBLS exampleFFAadd097)
  , ("FFA.Mul.337", exampleOutputBLS exampleFFAmul337)
  , ("FFA.Mul.097", exampleOutputBLS exampleFFAmul097)
  , ("FFA.Inv.337", exampleOutputBLS exampleFFAinv337)
  , ("FFA.Inv.097", exampleOutputBLS exampleFFAinv097)
  , ("Blake2b_224", exampleOutputBLS $ exampleBlake2b_224 @32)
  , ("Blake2b_256", exampleOutputBLS $ exampleBlake2b_256 @64)
  , ("SHA256.32", exampleOutputBLS $ exampleSHA @32)
  , ("MiMCHash", exampleOutputBLS exampleMiMC)
  , ("BLS12_381.Scale", exampleOutputBLS exampleBLS12_381Scale)
  , ("Ed25519.Scale", exampleOutput @(Zp Ed25519_Scalar) exampleEd25519Scale)
  , ("Fibonacci.100", exampleOutputBLS $ exampleFibonacci 100)
  , ("Reverse.32.3000", exampleOutputBLS $ exampleReverseList @32 @(ByteString 3000 (C _ _ _)))
  -- , ("ZkloginNoSig", exampleOutputBLS $ exampleZkLoginNoSig)
  -- , ("RSA.sign.verify.256", exampleOutputBLS exampleRSA)
  -- , ("JWT.secretBits", exampleOutputBLS $ exampleJWTSerialisation)
  -- , ("PedersonCommitment", exampleOutputBLS exampleCommitment)
  -- , ("BatchTransfer", exampleOutputBLS exampleBatchTransfer)
  ]
