{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples (ExampleOutput (..), examples) where

import Control.DeepSeq (NFData1)
import Data.Function (const, ($), (.))
import Data.Functor.Rep (Rep, Representable)
import Data.String (String)
import Data.Type.Equality (type (~))
import GHC.Generics (type (:*:))
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.EllipticCurve.Pasta (FpModulus)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Data.ByteString (Binary)
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Compiler (compile)
import ZkFold.Symbolic.Compiler.ArithmeticCircuit (ArithmeticCircuit)
import ZkFold.Symbolic.Compiler.ArithmeticCircuit.Context (CircuitContext)
import ZkFold.Symbolic.Data.Bool (true)
import ZkFold.Symbolic.Data.ByteString (ByteString)
import ZkFold.Symbolic.Data.Class (SymbolicData (..), SymbolicFunction (..), symFunc0, symFunc1, symFunc2, symFunc3)
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto))
import ZkFold.Symbolic.Data.Input (SymbolicInput)

import ZkFold.Symbolic.Examples.Blake2b (exampleBlake2b_224, exampleBlake2b_256)
import ZkFold.Symbolic.Examples.ByteString
import ZkFold.Symbolic.Examples.Conditional (exampleConditional)
import ZkFold.Symbolic.Examples.Constant
import ZkFold.Symbolic.Examples.ECDSA (exampleECDSA)
import ZkFold.Symbolic.Examples.Eq (exampleEq, exampleEqVector)
import ZkFold.Symbolic.Examples.FFA
import ZkFold.Symbolic.Examples.Fibonacci (exampleFibonacciMod)
import ZkFold.Symbolic.Examples.FieldElement (exampleInvert)
import ZkFold.Symbolic.Examples.LEQ (exampleLEQ)
import ZkFold.Symbolic.Examples.MerkleTree (exampleMerkleTree)
import ZkFold.Symbolic.Examples.MiMCHash (exampleMiMC)
import ZkFold.Symbolic.Examples.Pasta (examplePallas_Add, examplePallas_Scale)
import ZkFold.Symbolic.Examples.ReverseList (exampleReverseList)
import ZkFold.Symbolic.Examples.SmartWallet (expModContract)
import ZkFold.Symbolic.Examples.UInt

type A = Zp BLS12_381_Scalar

type B = Zp FpModulus

type C a = ArithmeticCircuit a

data ExampleOutput where
  ExampleOutput
    :: forall a i o
     . (Representable i, Binary (Rep i), NFData1 o, Arithmetic a)
    => (() -> C a i o)
    -> ExampleOutput

exampleOutput
  :: forall a i o c f
   . ( SymbolicFunction f
     , c ~ CircuitContext a
     , ContextF f ~ c
     , Domain f ~ o
     , SymbolicInput (Support f)
     , Context (Support f) ~ c
     , i ~ Payload (Support f) :*: Layout (Support f)
     , NFData1 o
     , Binary a
     )
  => f
  -> ExampleOutput
exampleOutput = ExampleOutput @a @i @o . const . compile

examples :: [(String, ExampleOutput)]
examples =
  [ ("Const", exampleOutput @A $ symFunc0 exampleConst)
  , ("Invert", exampleOutput @A $ symFunc1 exampleInvert) -- TODO: should be 1 constraint, 1 variable
  , ("Eq", exampleOutput @A $ symFunc2 exampleEq) -- TODO: should be 3 constraints, 3 variables
  , ("Eq.Const", exampleOutput @A $ symFunc1 exampleEqConst) -- TODO: should be 2 constraints, 2 variables
  , ("Eq.Vector", exampleOutput @A $ symFunc2 $ exampleEqVector @1) -- TODO: should be 3 constraints, 3 variables
  , ("Eq.Vector.Const", exampleOutput @A $ symFunc1 $ exampleEqVectorConst @1) -- TODO: should be 2 constraints, 2 variables
  , ("Conditional", exampleOutput @A $ symFunc3 exampleConditional) -- TODO: should be 4 constraints, 3 variables
  , ("Conditional.True", exampleOutput @A $ symFunc2 $ exampleConditional true) -- TODO: should be 0 constraints, 0 variables
  , ("Conditional.Const", exampleOutput @A $ symFunc2 exampleConditionalConst) -- TODO: should be 1 constraint, 1 variable
  , ("Conditional.Const.Const", exampleOutput @A $ symFunc1 exampleConditionalConstConst)
  , ("FibonacciMod.100", exampleOutput @A $ symFunc2 exampleFibonacciMod 100) -- TODO: rework after the IVC implementation
  , ("Reverse.32.3000", exampleOutput @A $ symFunc1 $ exampleReverseList @32 @(ByteString 3000 (CircuitContext _))) -- TODO: should be 3000*32 == 96000 constraints
  , ("LEQ", exampleOutput @A $ symFunc2 exampleLEQ)
  , ("ByteString.And.32", exampleOutput @A $ symFunc2 $ exampleByteStringAnd @32)
  , ("ByteString.Or.64", exampleOutput @A $ symFunc2 $ exampleByteStringOr @64)
  , ("ByteString.Extend.1.512", exampleOutput @A $ symFunc1 $ exampleByteStringResize @1 @512)
  , ("ByteString.Truncate.512.1", exampleOutput @A $ symFunc1 $ exampleByteStringResize @512 @1)
  , ("ByteString.Truncate.74.54", exampleOutput @A $ symFunc1 $ exampleByteStringResize @74 @54)
  , ("ByteString.Add.512", exampleOutput @A $ symFunc2 $ exampleByteStringAdd @512)
  , ("UInt.Extend.1.512", exampleOutput @A $ symFunc1 $ exampleUIntResize @1 @512 @Auto)
  , ("UInt.Truncate.512.1", exampleOutput @A $ symFunc1 $ exampleUIntResize @512 @1 @Auto)
  , ("UInt.Truncate.74.54", exampleOutput @A $ symFunc1 $ exampleUIntResize @74 @54 @Auto)
  , ("UInt.StrictAdd.256.Auto", exampleOutput @A $ symFunc2 $ exampleUIntStrictAdd @256 @Auto)
  , ("UInt.StrictMul.512.Auto", exampleOutput @A $ symFunc2 $ exampleUIntStrictMul @512 @Auto)
  , ("UInt.Mul.64.Auto", exampleOutput @A $ symFunc2 $ exampleUIntMul @64 @Auto)
  , ("UInt.Mul.4096.Auto", exampleOutput @A $ symFunc2 $ exampleUIntMul @4096 @Auto)
  , ("UInt.LEQ.256.Auto", exampleOutput @A $ symFunc2 $ exampleUIntLeq @256 @Auto)
  , ("UInt.ProductMod.1024.Auto", exampleOutput @A $ symFunc3 $ exampleUIntProductMod @1024 @Auto)
  , ("UInt.DivMod.32.Auto", exampleOutput @A $ symFunc2 $ exampleUIntDivMod @32 @Auto)
  , ("UInt.ExpMod.32.16.64.Auto", exampleOutput @A $ symFunc3 $ exampleUIntExpMod @32 @16 @64 @Auto)
  , ("UInt.ExpMod.256.64.1024.Auto", exampleOutput @A $ symFunc3 $ exampleUIntExpMod @256 @64 @1024 @Auto)
  , ("FFA.Add.Native", exampleOutput @B $ symFunc2 exampleFFAaddNative)
  , ("FFA.Mul.Native", exampleOutput @B $ symFunc2 exampleFFAmulNative)
  , ("FFA.Inv.Native", exampleOutput @B $ symFunc1 exampleFFAinvNative)
  , ("FFA.Add.Foreign", exampleOutput @A $ symFunc2 exampleFFAaddForeign)
  , ("FFA.Mul.Foreign", exampleOutput @A $ symFunc2 exampleFFAmulForeign)
  , ("FFA.Inv.Foreign", exampleOutput @A $ symFunc1 exampleFFAinvForeign)
  , ("Pallas.Add", exampleOutput @B $ symFunc2 examplePallas_Add)
  , ("Pallas.Scale", exampleOutput @B $ symFunc2 examplePallas_Scale)
  , -- , ("Jubjub.Scale", exampleOutput @A exampleJubjubScale)
    -- , ("Ed25519.Scale", exampleOutput @(Zp Ed25519_Base) exampleEd25519Scale)
    -- , ("BLS12_381.Scale", exampleOutput @A exampleBLS12_381Scale)
    ("ECDSA.Pallas.256", exampleOutput @B $ symFunc3 exampleECDSA)
  , -- , ("Mithril.256.2", exampleOutput @B $ exampleMithril @256 @2)
    ("Blake2b_224", exampleOutput @A $ symFunc1 $ exampleBlake2b_224 @32)
  , ("Blake2b_256", exampleOutput @A $ symFunc1 $ exampleBlake2b_256 @64)
  , ("SHA256.32", exampleOutput @A $ symFunc1 $ exampleSHA @32)
  , ("MiMCHash", exampleOutput @A $ symFunc2 exampleMiMC)
  , ("MerkleTree.4", exampleOutput @A $ symFunc2 $ exampleMerkleTree @4)
  , ("Exp65537Mod", exampleOutput @A $ symFunc1 expModContract)
  -- , ("RSA.sign.verify.256", exampleOutput @A exampleRSA)
  -- , ("JWT.secretBits", exampleOutput @A $ exampleJWTSerialisation)
  -- , ("ZkloginNoSig", exampleOutput @A $ exampleZkLoginNoSig)
  ]
