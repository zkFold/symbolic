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
type C = ArithmeticCircuit A

data ExampleOutput where
  ExampleOutput ::
    forall p i o.
    (Representable p, Representable i, NFData (Rep i), NFData1 o) =>
    (() -> C p i o) -> ExampleOutput

exampleOutput ::
  forall p i o c f.
  ( SymbolicData f
  , c ~ C p i
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
exampleOutput = ExampleOutput @p @i @o . const . compile

examples :: [(String, ExampleOutput)]
examples =
  [ ("Constant.5", exampleOutput exampleConst5)
  , ("Eq.Constant.5", exampleOutput exampleEq5)
  , ("Eq", exampleOutput exampleEq)
  , ("Conditional", exampleOutput exampleConditional)
  , ("LEQ", exampleOutput exampleLEQ)
  , ("ByteString.And.32", exampleOutput $ exampleByteStringAnd @32)
  , ("ByteString.Or.64", exampleOutput $ exampleByteStringOr @64)
  , ("ByteString.Extend.1.512", exampleOutput $ exampleByteStringResize @1 @512)
  , ("ByteString.Truncate.512.1", exampleOutput $ exampleByteStringResize @512 @1)
  , ("ByteString.Truncate.74.54", exampleOutput $ exampleByteStringResize @74 @54)
  , ("ByteString.Add.512", exampleOutput $ exampleByteStringAdd @512)
  , ("UInt.Extend.1.512", exampleOutput $ exampleUIntResize @1 @512 @Auto)
  , ("UInt.Truncate.512.1", exampleOutput $ exampleUIntResize @512 @1 @Auto)
  , ("UInt.Truncate.74.54", exampleOutput $ exampleUIntResize @74 @54 @Auto)
  , ("UInt.StrictAdd.256.Auto", exampleOutput $ exampleUIntStrictAdd @256 @Auto)
  , ("UInt.StrictMul.512.Auto", exampleOutput $ exampleUIntStrictMul @512 @Auto)
  , ("UInt.Mul.64.Auto", exampleOutput $ exampleUIntMul @64 @Auto)
  , ("UInt.Mul.4096.Auto", exampleOutput $ exampleUIntMul @4096 @Auto)
  , ("UInt.ProductMod.1024.Auto", exampleOutput $ exampleUIntProductMod @1024 @Auto)
  , ("UInt.DivMod.32.Auto", exampleOutput $ exampleUIntDivMod @32 @Auto)
  , ("UInt.ExpMod.32.16.64.Auto", exampleOutput $ exampleUIntExpMod @32 @16 @64 @Auto)
  , ("UInt.ExpMod.256.64.1024.Auto", exampleOutput $ exampleUIntExpMod @256 @64 @1024 @Auto)
  , ("UInt.LEQ.256.Auto", exampleOutput $ exampleUIntLeq @256 @Auto)
  , ("FFA.Add.337", exampleOutput exampleFFAadd337)
  , ("FFA.Add.097", exampleOutput exampleFFAadd097)
  , ("FFA.Mul.337", exampleOutput exampleFFAmul337)
  , ("FFA.Mul.097", exampleOutput exampleFFAmul097)
  , ("FFA.Inv.337", exampleOutput exampleFFAinv337)
  , ("FFA.Inv.097", exampleOutput exampleFFAinv097)
  , ("Blake2b_224", exampleOutput $ exampleBlake2b_224 @32)
  , ("Blake2b_256", exampleOutput $ exampleBlake2b_256 @64)
  , ("SHA256.32", exampleOutput $ exampleSHA @32)
  , ("MiMCHash", exampleOutput exampleMiMC)
  -- , ("BLS12_381.Scale", exampleOutput exampleBLS12_381Scale)
  -- , ("Ed25519.Scale", exampleOutput exampleEd25519Scale)
  , ("Fibonacci.100", exampleOutput $ exampleFibonacci 100)
  , ("Reverse.32.3000", exampleOutput $ exampleReverseList @32 @(ByteString 3000 (C _ _)))
  -- , ("ZkloginNoSig", exampleOutput $ exampleZkLoginNoSig)
  -- , ("RSA.sign.verify.256", exampleOutput exampleRSA)
  -- , ("JWT.secretBits", exampleOutput $ exampleJWTSerialisation)
  -- , ("PedersonCommitment", exampleOutput exampleCommitment)
  -- , ("BatchTransfer", exampleOutput exampleBatchTransfer)
  ]
