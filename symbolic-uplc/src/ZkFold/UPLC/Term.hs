{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE PolyKinds #-}

module ZkFold.UPLC.Term where

import           Data.Bool                                   (Bool)
import           Data.ByteString                             (ByteString)
import           Data.Text                                   (Text)
import           Data.Word                                   (Word64)
import           Numeric.Natural                             (Natural)
import           Prelude                                     (Integer)

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point, BLS12_381_G2_Point)
import           ZkFold.UPLC.BuiltinFunction                 (BuiltinFunction)
import           ZkFold.UPLC.BuiltinType                     (BuiltinType (..))

-- | Constructor tags used on Cardano.
--
-- While theoretically unbounded, in practice it should fit in 64 bits as said in [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf).
type ConstructorTag = Word64

-- | Variables in UPLC terms.
--
-- While theoretically unspecified, binary representation of terms on Cardano
-- uses [De Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index), as said in [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf).
-- They also are most convenient for Converter, so we use them, too.
type DeBruijnIndex = Natural

-- | Plutus Core's Data builtin type as a regular Haskell datatype.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf).
data Data
  = DConstr ConstructorTag [Data]
  | DMap [(Data, Data)]
  | DList [Data]
  | DI Integer
  | DB ByteString

-- | Constants available in Plutus Core.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf).
--
-- Note that all constants are annotated with a corresponding 'BuiltinType'
-- to avoid implementation errors.
data Constant (t :: BuiltinType) where
  CInteger :: Integer -> Constant BTInteger
  CByteString :: ByteString -> Constant BTByteString
  CString :: Text -> Constant BTString
  CBool :: Bool -> Constant BTBool
  CUnit :: () -> Constant BTUnit
  CData :: Data -> Constant BTData
  CList :: [Constant t] -> Constant (BTList t)
  CPair :: Constant t -> Constant u -> Constant (BTPair t u)
  CG1 :: BLS12_381_G1_Point -> Constant BTBLSG1
  CG2 :: BLS12_381_G2_Point -> Constant BTBLSG2

-- | Terms of Plutus Core as a Haskell datatype.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf).
data Term
  = TVariable !DeBruijnIndex
  | forall t. TConstant !(Constant t)
  | forall s t. TBuiltin !(BuiltinFunction s t)
  | TLam {- DeBruijnIndex = 0 -} Term
  | TApp Term Term
  | TDelay Term
  | TForce Term
  | TConstr !ConstructorTag [Term]
  | TCase Term [Term]
  | TError
