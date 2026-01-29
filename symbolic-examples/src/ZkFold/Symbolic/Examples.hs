{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples (ExampleOutput (..), examples) where

import Control.DeepSeq (NFData)
import Data.Function (($))
import Data.String (String)
import Data.Traversable (Traversable)
import Data.Type.Equality (type (~))
import GHC.Generics (U1)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.EllipticCurve.Pasta (FpModulus)
import ZkFold.Algebra.Field (Zp)
import ZkFold.ArithmeticCircuit.Elem (Elem)
import ZkFold.ArithmeticCircuit.Node (Input, Output, SymbolicFunction, apply)
import ZkFold.ArithmeticCircuit.Var (Var)
import ZkFold.Data.Binary (Binary)
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Data.Bool (true)
import ZkFold.Symbolic.Data.ByteString (ByteString)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
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
     . ( Arithmetic a
       , Binary a
       , NFData a
       , SymbolicInput i
       , HasRep i (Elem a)
       , SymbolicData o
       , Input (o (Elem a)) ~ U1
       , Output (o (Elem a)) ~ o
       , NFData (Layout o (Elem a) a)
       , NFData (Layout o (Elem a) (Var a))
       , Traversable (Layout o (Elem a))
       )
    => (i (Elem a) -> o (Elem a))
    -> ExampleOutput

exampleOutput
  :: forall a f
   . ( Arithmetic a
     , Binary a
     , NFData a
     , SymbolicFunction (Elem a) f
     , NFData (Layout (Output f) (Elem a) a)
     , NFData (Layout (Output f) (Elem a) (Var a))
     , Input (Output f (Elem a)) ~ U1
     , Output (Output f (Elem a)) ~ Output f
     )
  => f
  -> ExampleOutput
exampleOutput f = ExampleOutput (apply f)

examples :: [(String, ExampleOutput)]
examples =
  [ ("Const", exampleOutput @A exampleConst)
  , ("Invert", exampleOutput @A $ exampleInvert @(Elem A)) -- TODO: should be 1 constraint, 1 variable
  , ("Eq", exampleOutput @A $ exampleEq @(Elem A)) -- TODO: should be 3 constraints, 3 variables
  , ("Eq.Const", exampleOutput @A $ exampleEqConst @(Elem A)) -- TODO: should be 2 constraints, 2 variables
  , ("Eq.Vector", exampleOutput @A $ exampleEqVector @1 @(Elem A)) -- TODO: should be 3 constraints, 3 variables
  , ("Eq.Vector.Const", exampleOutput @A $ exampleEqVectorConst @1 @(Elem A)) -- TODO: should be 2 constraints, 2 variables
  , ("Conditional", exampleOutput @A $ exampleConditional @(Elem A)) -- TODO: should be 4 constraints, 3 variables
  , ("Conditional.True", exampleOutput @A $ exampleConditional @(Elem A) true) -- TODO: should be 0 constraints, 0 variables
  , ("Conditional.Const", exampleOutput @A $ exampleConditionalConst @(Elem A)) -- TODO: should be 1 constraint, 1 variable
  , ("Conditional.Const.Const", exampleOutput @A $ exampleConditionalConstConst @(Elem A))
  , ("FibonacciMod.100", exampleOutput @A $ exampleFibonacciMod @(Elem A) 100) -- TODO: rework after the IVC implementation
  , ("Reverse.32.3000", exampleOutput @A $ exampleReverseList @32 @(ByteString 3000) @(Elem A)) -- TODO: should be 3000*32 == 96000 constraints
  , ("LEQ", exampleOutput @A $ exampleLEQ @(Elem A))
  , ("ByteString.And.32", exampleOutput @A $ exampleByteStringAnd @32 @(Elem A))
  , ("ByteString.Or.64", exampleOutput @A $ exampleByteStringOr @64 @(Elem A))
  , ("ByteString.Extend.1.512", exampleOutput @A $ exampleByteStringResize @1 @512 @(Elem A))
  , ("ByteString.Truncate.512.1", exampleOutput @A $ exampleByteStringResize @512 @1 @(Elem A))
  , ("ByteString.Truncate.74.54", exampleOutput @A $ exampleByteStringResize @74 @54 @(Elem A))
  , ("ByteString.Add.512", exampleOutput @A $ exampleByteStringAdd @512 @(Elem A))
  , ("UInt.Extend.1.512", exampleOutput @A $ exampleUIntResize @1 @512 @(Elem A))
  , ("UInt.Truncate.512.1", exampleOutput @A $ exampleUIntResize @512 @1 @(Elem A))
  , ("UInt.Truncate.74.54", exampleOutput @A $ exampleUIntResize @74 @54 @(Elem A))
  , ("UInt.StrictAdd.256.Auto", exampleOutput @A $ exampleUIntStrictAdd @256 @(Elem A))
  , ("UInt.StrictMul.512.Auto", exampleOutput @A $ exampleUIntStrictMul @512 @(Elem A))
  , ("UInt.Mul.64.Auto", exampleOutput @A $ exampleUIntMul @64 @(Elem A))
  , ("UInt.Mul.4096.Auto", exampleOutput @A $ exampleUIntMul @4096 @(Elem A))
  , ("UInt.LEQ.256.Auto", exampleOutput @A $ exampleUIntLeq @256 @(Elem A))
  , ("UInt.DivMod.32.Auto", exampleOutput @A $ exampleUIntDivMod @32 @(Elem A))
  , ("UInt.ExpMod.32.16.64.Auto", exampleOutput @A $ exampleUIntExpMod @32 @16 @64 @(Elem A))
  , ("UInt.ExpMod.256.64.1024.Auto", exampleOutput @A $ exampleUIntExpMod @256 @64 @1024 @(Elem A))
  , ("FFA.Add.Native", exampleOutput @B $ exampleFFAaddNative @(Elem B))
  , ("FFA.Mul.Native", exampleOutput @B $ exampleFFAmulNative @(Elem B))
  , ("FFA.Inv.Native", exampleOutput @B $ exampleFFAinvNative @(Elem B))
  , ("FFA.Add.Foreign", exampleOutput @A $ exampleFFAaddForeign @(Elem A))
  , ("FFA.Mul.Foreign", exampleOutput @A $ exampleFFAmulForeign @(Elem A))
  , ("FFA.Inv.Foreign", exampleOutput @A $ exampleFFAinvForeign @(Elem A))
  , ("Pallas.Add", exampleOutput @B $ examplePallas_Add @(Elem B))
  , ("Pallas.Scale", exampleOutput @B $ examplePallas_Scale @(Elem B))
  , -- , ("Jubjub.Scale", exampleOutput @A exampleJubjubScale)
    -- , ("Ed25519.Scale", exampleOutput @(Zp Ed25519_Base) exampleEd25519Scale)
    -- , ("BLS12_381.Scale", exampleOutput @A exampleBLS12_381Scale)
    ("ECDSA.Pallas.256", exampleOutput @B $ exampleECDSA @(Elem B))
  , -- , ("Mithril.256.2", exampleOutput @B $ exampleMithril @256 @2)
    ("Blake2b_224", exampleOutput @A $ exampleBlake2b_224 @32 @(Elem A))
  , ("Blake2b_256", exampleOutput @A $ exampleBlake2b_256 @64 @(Elem A))
  , ("SHA256.32", exampleOutput @A $ exampleSHA @32 @(Elem A))
  , ("MiMCHash", exampleOutput @A $ exampleMiMC @(Elem A))
  , ("Exp65537Mod", exampleOutput @A $ expModContract @(Elem A))
  -- , ("RSA.sign.verify.256", exampleOutput @A exampleRSA)
  -- , ("JWT.secretBits", exampleOutput @A $ exampleJWTSerialisation)
  -- , ("ZkloginNoSig", exampleOutput @A $ exampleZkLoginNoSig)
  ]
