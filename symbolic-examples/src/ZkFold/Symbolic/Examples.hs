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
import           Examples.Pasta                              (examplePallas_Add, examplePallas_Scale)
import           Examples.ReverseList                        (exampleReverseList)
import           Examples.UInt
import           GHC.Generics                                (type (:*:))

import           ZkFold.Base.Algebra.Basic.Field             (Zp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import           ZkFold.Base.Algebra.EllipticCurve.Pasta     (FpModulus)
import           ZkFold.Base.Data.ByteString                 (Binary)
import           ZkFold.Symbolic.Class                       (Arithmetic)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit, compile)
import           ZkFold.Symbolic.Data.ByteString             (ByteString)
import           ZkFold.Symbolic.Data.Class                  (SymbolicData (..))
import           ZkFold.Symbolic.Data.Combinators            (RegisterSize (Auto))
import           ZkFold.Symbolic.Data.Input                  (SymbolicInput)

type A = Zp BLS12_381_Scalar
type B = Zp FpModulus
type C a = ArithmeticCircuit a

data ExampleOutput where
  ExampleOutput ::
    forall a i o.
    (Representable i, NFData (Rep i), NFData1 o, Arithmetic a, Binary a) =>
    (() -> C a i o) -> ExampleOutput

exampleOutput ::
  forall a i o c f.
  ( SymbolicData f
  , c ~ C a i
  , Context f ~ c
  , Layout f ~ o
  , SymbolicInput (Support f)
  , Context (Support f) ~ c
  , i ~ Payload (Support f) :*: Layout (Support f)
  , NFData1 o
  , Binary a
  ) => f -> ExampleOutput
exampleOutput = ExampleOutput @a @i @o . const . compile

examples :: [(String, ExampleOutput)]
examples =
  [ ("Constant.5", exampleOutput @A exampleConst5)
  , ("Eq.Constant.5", exampleOutput @A exampleEq5)
  , ("Eq", exampleOutput @A exampleEq)
  , ("Conditional", exampleOutput @A exampleConditional)
  , ("LEQ", exampleOutput @A exampleLEQ)
  , ("ByteString.And.32", exampleOutput @A $ exampleByteStringAnd @32)
  , ("ByteString.Or.64", exampleOutput @A $ exampleByteStringOr @64)
  , ("ByteString.Extend.1.512", exampleOutput @A $ exampleByteStringResize @1 @512)
  , ("ByteString.Truncate.512.1", exampleOutput @A $ exampleByteStringResize @512 @1)
  , ("ByteString.Truncate.74.54", exampleOutput @A $ exampleByteStringResize @74 @54)
  , ("ByteString.Add.512", exampleOutput @A $ exampleByteStringAdd @512)
  , ("UInt.Extend.1.512", exampleOutput @A $ exampleUIntResize @1 @512 @Auto)
  , ("UInt.Truncate.512.1", exampleOutput @A $ exampleUIntResize @512 @1 @Auto)
  , ("UInt.Truncate.74.54", exampleOutput @A $ exampleUIntResize @74 @54 @Auto)
  , ("UInt.StrictAdd.256.Auto", exampleOutput @A $ exampleUIntStrictAdd @256 @Auto)
  , ("UInt.StrictMul.512.Auto", exampleOutput @A $ exampleUIntStrictMul @512 @Auto)
  , ("UInt.Mul.64.Auto", exampleOutput @A $ exampleUIntMul @64 @Auto)
  , ("UInt.Mul.4096.Auto", exampleOutput @A $ exampleUIntMul @4096 @Auto)
  , ("UInt.LEQ.256.Auto", exampleOutput @A $ exampleUIntLeq @256 @Auto)
  , ("UInt.ProductMod.1024.Auto", exampleOutput @A $ exampleUIntProductMod @1024 @Auto)
  , ("UInt.DivMod.32.Auto", exampleOutput @A $ exampleUIntDivMod @32 @Auto)
  , ("UInt.ExpMod.32.16.64.Auto", exampleOutput @A $ exampleUIntExpMod @32 @16 @64 @Auto)
  , ("UInt.ExpMod.256.64.1024.Auto", exampleOutput @A $ exampleUIntExpMod @256 @64 @1024 @Auto)
  , ("FFA.Add.097", exampleOutput @A exampleFFAadd097)
  , ("FFA.Mul.097", exampleOutput @A exampleFFAmul097)
  , ("FFA.Inv.097", exampleOutput @A exampleFFAinv097)
  , ("FFA.Add.Native", exampleOutput @B exampleFFAadd337)
  , ("FFA.Mul.Native", exampleOutput @B exampleFFAmul337)
  , ("FFA.Inv.Native", exampleOutput @B exampleFFAinv337)
  , ("Pallas.Add", exampleOutput @B examplePallas_Add)
  , ("Pallas.Scale", exampleOutput @B examplePallas_Scale)
  -- , ("BLS12_381.Scale", exampleOutput @A exampleBLS12_381Scale)
  -- , ("Ed25519.Scale", exampleOutput @(Zp Ed25519_Base) exampleEd25519Scale)
  -- , ("ECDSA.Pallas.256", exampleOutput @B exampleECDSA)
  -- , ("Mithril.256.2", exampleOutput @B $ exampleMithril @256 @2)
  , ("Blake2b_224", exampleOutput @A $ exampleBlake2b_224 @32)
  , ("Blake2b_256", exampleOutput @A $ exampleBlake2b_256 @64)
  , ("SHA256.32", exampleOutput @A $ exampleSHA @32)
  , ("MiMCHash", exampleOutput @A exampleMiMC)
  , ("Fibonacci.100", exampleOutput @A $ exampleFibonacci 100)
  , ("Reverse.32.3000", exampleOutput @A $ exampleReverseList @32 @(ByteString 3000 (C _ _)))
  -- , ("ZkloginNoSig", exampleOutput @A $ exampleZkLoginNoSig)
  -- , ("RSA.sign.verify.256", exampleOutput @A exampleRSA)
  -- , ("JWT.secretBits", exampleOutput @A $ exampleJWTSerialisation)
  -- , ("PedersonCommitment", exampleOutput @A exampleCommitment)
  -- , ("BatchTransfer", exampleOutput @A exampleBatchTransfer)
  ]
