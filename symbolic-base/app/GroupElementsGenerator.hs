module Main (main) where

import           Data.Aeson                             (ToJSON, encode)
import           Data.List                              (intercalate)
import qualified Data.ByteString.Lazy.Char8             as BL
import           Numeric.Natural                        (Natural)
import           Prelude
import           System.Environment                     (getArgs)
import           Text.Read                              (readMaybe)

import           ZkFold.Algebra.Class                   (Scale(..), FromConstant(..))
import           ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import           ZkFold.Algebra.EllipticCurve.BN254     (BN254_G1_Point)
import           ZkFold.Algebra.EllipticCurve.Class     (CyclicGroup(..))

-- | Supported groups
data Group = BN254_G1 | BLS12_381_G1
    deriving (Show, Eq, Enum, Bounded)

-- | Output format
data OutputFormat = Plain | JSON
    deriving (Eq, Show)

-- | Canonical string name for each group
groupName :: Group -> String
groupName BN254_G1     = "bn254-g1"
groupName BLS12_381_G1 = "bls12381-g1"

-- | Parse group from string
parseGroup :: String -> Maybe Group
parseGroup s = lookup s [(groupName g, g) | g <- [minBound .. maxBound :: Group]]

-- | Parse output format from args
parseFormat :: [String] -> OutputFormat
parseFormat args | "--json" `elem` args = JSON
                 | otherwise            = Plain

-- | Print usage/help
printUsage :: IO ()
printUsage =
  let groupList = map groupName [minBound .. maxBound :: Group]
      groupListStr = intercalate " | " groupList
  in putStrLn $ unlines
    [ "Usage: group-elements-generator <group> <x> <length> [--json]"
    , "  <group>: " <> groupListStr
    , "  <x>:     group exponent seed (natural number)"
    , "  <length>: number of elements to generate (integer)"
    , "  --json:  output as JSON array of points"
    , "  -h, --help: show this help message"
    , "Example: group-elements-generator " <> head groupList <> " 2 5 --json"
    ]

main :: IO ()
main = do
  args <- getArgs
  let help = any (`elem` ["-h", "--help"]) args
  if help then printUsage else
    case args of
      (groupStr:xStr:nStr:rest) ->
        case parseGroup groupStr of
          Just g -> case g of
            BN254_G1     -> runGroupElementsGeneric g (pointGen :: BN254_G1_Point) xStr nStr (parseFormat rest)
            BLS12_381_G1 -> runGroupElementsGeneric g (pointGen :: BLS12_381_G1_Point) xStr nStr (parseFormat rest)
          Nothing ->
            putStrLn $ "Unknown group: " <> groupStr <>
              "\nSupported groups: " <> show (map groupName [minBound .. maxBound :: Group])
      _ -> printUsage

runGroupElementsGeneric :: forall pt.
  ( Scale (ScalarFieldOf pt) pt
  , FromConstant Natural (ScalarFieldOf pt)
  , Show pt
  , ToJSON pt
  )
  => Group -> pt -> String -> String -> OutputFormat -> IO ()
runGroupElementsGeneric group g xStr nStr fmt =
  case (readMaybe xStr :: Maybe Natural, readMaybe nStr :: Maybe Int) of
    (Just x, Just n) -> do
      let exps = [x ^ (fromIntegral k :: Natural) | k <- [1..n]]
          points = [scale (fromConstant @_ @(ScalarFieldOf pt) e) g | e <- exps]
      case fmt of
        Plain -> do
          putStrLn $ "Generated group points (" <> groupName group <> "):"
          mapM_ print points
        JSON -> BL.putStrLn (encode points)
    _ -> putStrLn "Error: Invalid input. Please provide a valid natural number for <x> and an integer for <length>."
