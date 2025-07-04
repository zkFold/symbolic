{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples (ExampleOutput (..), examples) where

import Control.DeepSeq (NFData)
import Data.Function (($))
import Data.String (String)
import Data.Type.Equality (type (~))
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.EllipticCurve.Pasta (FpModulus)
import ZkFold.Algebra.Field (Zp)
import ZkFold.ArithmeticCircuit.Context (CircuitContext)
import ZkFold.Data.ByteString (Binary)
import ZkFold.Symbolic.Data.Bool (true)
import ZkFold.Symbolic.Data.ByteString (ByteString)
import ZkFold.Symbolic.Data.Class (Domain, Range, SymbolicData (..), SymbolicFunction (..))
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

data ExampleOutput where
  ExampleOutput
    :: forall a i o
     . ( Binary a
       , SymbolicInput i
       , Context i ~ CircuitContext a
       , SymbolicData o
       , Context o ~ CircuitContext a
       , NFData (Layout o a)
       )
    => (i -> o)
    -> ExampleOutput

exampleOutput
  :: forall a f
   . ( Binary a
     , SymbolicFunction f
     , SymbolicInput (Domain f)
     , Context (Domain f) ~ CircuitContext a
     , SymbolicData (Range f)
     , Context (Range f) ~ CircuitContext a
     , NFData (Layout (Range f) a)
     )
  => f
  -> ExampleOutput
exampleOutput f = ExampleOutput (apply f)

examples :: [(String, ExampleOutput)]
examples =
  [ ("Const", exampleOutput @A exampleConst)
  , ("Invert", exampleOutput @A exampleInvert) -- TODO: should be 1 constraint, 1 variable
  , ("Eq", exampleOutput @A exampleEq) -- TODO: should be 3 constraints, 3 variables
  , ("Eq.Const", exampleOutput @A exampleEqConst) -- TODO: should be 2 constraints, 2 variables
  , ("Eq.Vector", exampleOutput @A $ exampleEqVector @1) -- TODO: should be 3 constraints, 3 variables
  , ("Eq.Vector.Const", exampleOutput @A $ exampleEqVectorConst @1) -- TODO: should be 2 constraints, 2 variables
  , ("Conditional", exampleOutput @A exampleConditional) -- TODO: should be 4 constraints, 3 variables
  , ("Conditional.True", exampleOutput @A $ exampleConditional true) -- TODO: should be 0 constraints, 0 variables
  , ("Conditional.Const", exampleOutput @A exampleConditionalConst) -- TODO: should be 1 constraint, 1 variable
  , ("Conditional.Const.Const", exampleOutput @A exampleConditionalConstConst)
  , ("FibonacciMod.100", exampleOutput @A $ exampleFibonacciMod 100) -- TODO: rework after the IVC implementation
  , ("Reverse.32.3000", exampleOutput @A $ exampleReverseList @32 @(ByteString 3000 (CircuitContext _))) -- TODO: should be 3000*32 == 96000 constraints
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
  , ("FFA.Add.Native", exampleOutput @B exampleFFAaddNative)
  , ("FFA.Mul.Native", exampleOutput @B exampleFFAmulNative)
  , ("FFA.Inv.Native", exampleOutput @B exampleFFAinvNative)
  , ("FFA.Add.Foreign", exampleOutput @A exampleFFAaddForeign)
  , ("FFA.Mul.Foreign", exampleOutput @A exampleFFAmulForeign)
  , ("FFA.Inv.Foreign", exampleOutput @A exampleFFAinvForeign)
  , ("Pallas.Add", exampleOutput @B examplePallas_Add)
  , ("Pallas.Scale", exampleOutput @B examplePallas_Scale)
  , -- , ("Jubjub.Scale", exampleOutput @A exampleJubjubScale)
    -- , ("Ed25519.Scale", exampleOutput @(Zp Ed25519_Base) exampleEd25519Scale)
    -- , ("BLS12_381.Scale", exampleOutput @A exampleBLS12_381Scale)
    ("ECDSA.Pallas.256", exampleOutput @B exampleECDSA)
  , -- , ("Mithril.256.2", exampleOutput @B $ exampleMithril @256 @2)
    ("Blake2b_224", exampleOutput @A $ exampleBlake2b_224 @32)
  , ("Blake2b_256", exampleOutput @A $ exampleBlake2b_256 @64)
  , ("SHA256.32", exampleOutput @A $ exampleSHA @32)
  , ("MiMCHash", exampleOutput @A exampleMiMC)
  , ("MerkleTree.4", exampleOutput @A $ exampleMerkleTree @4)
  , ("Exp65537Mod", exampleOutput @A expModContract)
  -- , ("RSA.sign.verify.256", exampleOutput @A exampleRSA)
  -- , ("JWT.secretBits", exampleOutput @A $ exampleJWTSerialisation)
  -- , ("ZkloginNoSig", exampleOutput @A $ exampleZkLoginNoSig)
  ]
