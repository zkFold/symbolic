{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-deprecated-type-abstractions #-}
{-# OPTIONS_GHC -Wno-unrecognised-warning-flags #-}

module Main (main) where

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (find, intercalate)
import Data.Maybe (fromJust)
import Data.Typeable (Proxy (..), Typeable, typeRep)
import Numeric.Natural (Natural)
import Options.Applicative
import ZkFold.Algebra.Class (FromConstant (..), Scale (..))
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point, BLS12_381_G2_Point)
import ZkFold.Algebra.EllipticCurve.BN254 (BN254_G1_Point, BN254_G2_Point)
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))
import Prelude

data Group = forall pt. (CyclicGroup pt, Show pt, ToJSON pt, Typeable pt) => Group

supportedGroups :: [(String, Group)]
supportedGroups =
  [ ("bn254-g1", Group @BN254_G1_Point)
  , ("bn254-g2", Group @BN254_G2_Point)
  , ("bls12381-g1", Group @BLS12_381_G1_Point)
  , ("bls12381-g2", Group @BLS12_381_G2_Point)
  ]

groupName :: Group -> String
groupName (Group @pt) = fst $ fromJust $ find f supportedGroups
 where
  f (_, g) = case g of
    Group @pt' -> typeRep (Proxy @pt) == typeRep (Proxy @pt')

-- | Output format
data OutputFormat = Plain | JSON
  deriving (Eq, Show)

-- | Parse group from string
parseGroup :: String -> Maybe Group
parseGroup s = lookup s supportedGroups

-- | CLI options
data Options = Options
  { optGroup :: Group
  , optSeed :: Natural
  , optLength :: Natural
  , optFormat :: OutputFormat
  }

optionsParser :: Parser Options
optionsParser =
  let groupList = map fst supportedGroups
      groupListStr = intercalate " | " groupList
      groupHelp = "Group name: one of { " <> groupListStr <> " }"
   in Options
        <$> option
          (maybeReader parseGroup)
          (long "group" <> short 'g' <> metavar "GROUP" <> help groupHelp)
        <*> option
          auto
          (long "seed" <> short 's' <> metavar "SEED" <> help "Exponent seed (natural number)")
        <*> option
          auto
          (long "length" <> short 'l' <> metavar "LENGTH" <> help "Number of elements to generate")
        <*> flag
          Plain
          JSON
          (long "json" <> help "Output as JSON array of points")

runGroupElementsGeneric
  :: forall pt
   . ( Scale (ScalarFieldOf pt) pt
     , FromConstant Natural (ScalarFieldOf pt)
     , Show pt
     , ToJSON pt
     )
  => Options -> pt -> IO ()
runGroupElementsGeneric opts g =
  let x = optSeed opts
      n = optLength opts
      fmt = optFormat opts
      group = optGroup opts
      exps = [x ^ k | k <- [0 .. n - 1]]
      points = [scale (fromConstant @_ @(ScalarFieldOf pt) e) g | e <- exps]
   in case fmt of
        Plain -> do
          putStrLn $ "Generated group points (" <> groupName group <> ") :"
          mapM_ print points
        JSON -> do
          let fname = groupName group <> "_n" <> show n <> ".json"
          BL.writeFile fname (encode points)
          putStrLn $ "Wrote JSON to " <> fname

main :: IO ()
main = do
  opts <-
    execParser $
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Generate group elements by multiplying the generator point by powers of a seed (from 0 to LENGTH-1)."
            <> header "Group Elements Generator"
        )
  case optGroup opts of
    Group @pt -> runGroupElementsGeneric opts (pointGen :: pt)
