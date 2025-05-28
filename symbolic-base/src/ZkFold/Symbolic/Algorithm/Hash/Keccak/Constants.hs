module ZkFold.Symbolic.Algorithm.Hash.Keccak.Constants (
  NumRounds,
  NumLanes,
  roundConstants,
  rotationConstants,
  piConstants,
  emptyState,
) where

import           Data.Function                   (($))
import           Data.Functor                    (Functor (..))
import           Data.Functor.Rep                (Representable (..))
import           GHC.IsList                      (fromList)
import           GHC.TypeNats                    (Natural)

import           ZkFold.Algebra.Class            (FromConstant (..))
import           ZkFold.Algebra.Field            (Zp)
import           ZkFold.Data.Vector              (Vector (..))
import           ZkFold.Symbolic.Class           (Symbolic)
import           ZkFold.Symbolic.Data.ByteString (ByteString)

-- | The number of rounds in the Keccak-f[1600] permutation.
type NumRounds :: Natural
type NumRounds = 24

-- | Number of lanes in the Keccak sponge state.
type NumLanes :: Natural
type NumLanes = 25

{- | Keccak round constants for the 24 rounds.

These are the first 64 bits of the binary expansion of the fractional parts of the cube roots of the first 24 prime numbers.
-}
roundConstants :: Symbolic context => Vector NumRounds (ByteString 64 context)
roundConstants =
  fromList $
    fmap
      fromConstant
      [ 0x0000000000000001 :: Natural
      , 0x0000000000008082
      , 0x800000000000808A
      , 0x8000000080008000
      , 0x000000000000808B
      , 0x0000000080000001
      , 0x8000000080008081
      , 0x8000000000008009
      , 0x000000000000008A
      , 0x0000000000000088
      , 0x0000000080008009
      , 0x000000008000000A
      , 0x000000008000808B
      , 0x800000000000008B
      , 0x8000000000008089
      , 0x8000000000008003
      , 0x8000000000008002
      , 0x8000000000000080
      , 0x000000000000800A
      , 0x800000008000000A
      , 0x8000000080008081
      , 0x8000000000008080
      , 0x0000000080000001
      , 0x8000000080008008
      ]

{- | Rotation offsets for the Keccak-f[1600] permutation.
These are the rotation offsets for each lane in the 5x5 state array.
-}
rotationConstants :: Vector NumLanes Natural
rotationConstants =
  fromList
    [ 0
    , 36
    , 3
    , 41
    , 18
    , 1
    , 44
    , 10
    , 45
    , 2
    , 62
    , 6
    , 43
    , 15
    , 61
    , 28
    , 55
    , 25
    , 21
    , 56
    , 27
    , 20
    , 39
    , 8
    , 14
    ]

{- | The pi constants for the Keccak-f[1600] permutation.

These constants define the lane permutation in the π step of Keccak-f[1600].
Each value represents the new position for the lane at that index in the 5x5 state array.
For example, the lane at position 1 moves to position 15, position 2 moves to position 5, etc.
-}
piConstants :: Vector NumLanes (Zp NumLanes)
piConstants =
  fromList
    [ 0
    , 15
    , 5
    , 20
    , 10
    , 6
    , 21
    , 11
    , 1
    , 16
    , 12
    , 2
    , 17
    , 7
    , 22
    , 18
    , 8
    , 23
    , 13
    , 3
    , 24
    , 14
    , 4
    , 19
    , 9
    ]

emptyState :: forall context. Symbolic context => Vector NumLanes (ByteString 64 context)
emptyState = tabulate (\_ -> fromConstant (0 :: Natural))
