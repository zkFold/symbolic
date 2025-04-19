module ZkFold.Symbolic.Algorithm.Hash.MiMC.Constants (mimcConstants) where

import           Crypto.Hash.SHA256              (hash)
import           Data.Maybe                      (fromJust)
import           Prelude

import           ZkFold.Algebra.Class (FromConstant (..))
import           ZkFold.Data.ByteString

mimcSeed :: LittleEndian
mimcSeed = 42

-- | The round constants ci are random elements of F_2n except for the first and
--   last round constants which are equal to 0.
--
mimcConstants :: forall a . FromConstant Integer a => [a]
mimcConstants =
  let
    getI = fromIntegral . fromJust . fromByteString @LittleEndian
    cs = take 218 $ map getI $ iterate hash $ toByteString mimcSeed
  in map (fromConstant @Integer @a) (0 : cs ++ [0])
