{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module ZkFold.Protocol.Halo2.Export (
  Halo2FieldExport (..),
  primeFieldDecimal,
  PlonkupLowering (..),
  lowerToPlonkup,
  AssignedCell (..),
  ImportedRow (..),
  LookupTableRow (..),
  Halo2CircuitIR (..),
  ExportError (..),
  exportHalo2Ir,
  writeHalo2IrFile,
) where

import Control.Applicative (pure)
import Data.Aeson qualified as Aeson
import Data.Binary (Binary, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (intToDigit)
import Data.Foldable (Foldable, toList)
import Data.Functor (fmap, (<$>))
import Data.Functor.Rep (Rep, Representable, tabulate)
import Data.List qualified as L
import Data.Map (elems, (!))
import Data.Map.Monoidal qualified as M
import Data.Maybe (Maybe (..), maybe)
import Data.Monoid (Sum (..))
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32, Word8)
import GHC.Generics (Generic)
import GHC.IsList (fromList)
import Numeric (showHex)
import Prelude (Bool, Either (..), Eq, FilePath, IO, Show, flip, foldMap, id, ($), (++), (.), (/=), (<>), (==))
import Prelude qualified as P

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number
import ZkFold.Algebra.Permutation (fromCycles, mkIndexPartition)
import ZkFold.Algebra.Polynomial.Multivariate (evalMonomial, evalPolynomial, var)
import ZkFold.Algebra.Polynomial.Univariate (UnivariateRingPolyVec (..), toPolyVec)
import ZkFold.ArithmeticCircuit (ArithmeticCircuit (..), acSizeN, witnessGenerator)
import ZkFold.ArithmeticCircuit.Context (CircuitContext (..), LookupFunction (..), LookupType (..))
import ZkFold.ArithmeticCircuit.Var (LinVar (..), Var, evalVar, toVar)
import ZkFold.Prelude (length, replicate, uncurry3)
import ZkFold.Protocol.Plonkup.Internal (PlonkupPermutationSize)
import ZkFold.Protocol.Plonkup.LookupConstraint (LookupConstraint (LookupConstraint))
import ZkFold.Protocol.Plonkup.PlonkConstraint (PlonkConstraint (..), toPlonkConstraint)
import ZkFold.Protocol.Plonkup.PlonkupConstraint
import ZkFold.Protocol.Plonkup.Relation (PlonkupRelation (..))
import ZkFold.Protocol.Plonkup.Relation.LookupVector (fromVector, toVector)
import ZkFold.Symbolic.Class (Arithmetic)

class Halo2FieldExport a where
  halo2FieldTag :: proxy a -> T.Text
  halo2FieldText :: a -> T.Text

primeFieldDecimal :: (PrimeField a, Show (IntegralOf a)) => a -> T.Text
primeFieldDecimal = T.pack . P.show . toIntegral

data PlonkupLowering i o n a pv = PlonkupLowering
  { relation :: !(PlonkupRelation i o n a pv)
  , wireA :: !(Vector (Var a))
  , wireB :: !(Vector (Var a))
  , wireC :: !(Vector (Var a))
  , lookupTableRows :: !(Vector (a, a, a))
  }
  deriving Generic

data AssignedCell = AssignedCell
  { cellValue :: !T.Text
  , equalityKey :: !(Maybe T.Text)
  }
  deriving (Aeson.FromJSON, Aeson.ToJSON, Eq, Generic, Show)

data ImportedRow = ImportedRow
  { rowIndex :: !Word32
  , aCell :: !AssignedCell
  , bCell :: !AssignedCell
  , cCell :: !AssignedCell
  , qMRow :: !T.Text
  , qLRow :: !T.Text
  , qRRow :: !T.Text
  , qORow :: !T.Text
  , qCRow :: !T.Text
  , qLookup :: !Bool
  , instanceIndex :: !(Maybe Word32)
  }
  deriving (Aeson.FromJSON, Aeson.ToJSON, Eq, Generic, Show)

data LookupTableRow = LookupTableRow
  { t1 :: !T.Text
  , t2 :: !T.Text
  , t3 :: !T.Text
  }
  deriving (Aeson.FromJSON, Aeson.ToJSON, Eq, Generic, Show)

data Halo2CircuitIR = Halo2CircuitIR
  { formatVersion :: !Word32
  , fieldTag :: !T.Text
  , rows :: ![ImportedRow]
  , lookupTable :: ![LookupTableRow]
  , publicInputs :: ![T.Text]
  }
  deriving (Aeson.FromJSON, Aeson.ToJSON, Eq, Generic, Show)

data ExportError
  = RelationTooSmall
  deriving (Aeson.FromJSON, Aeson.ToJSON, Eq, Generic, Show)

writeHalo2IrFile :: FilePath -> Halo2CircuitIR -> IO ()
writeHalo2IrFile fp ir = Aeson.encodeFile fp ir

lowerToPlonkup
  :: forall i o n a pv
   . ( KnownNat n
     , Arithmetic a
     , Binary (Rep i)
     , UnivariateRingPolyVec a pv
     , Representable i
     , Representable o
     , Foldable o
     )
  => ArithmeticCircuit a i o
  -> Maybe (PlonkupLowering i o n a pv)
lowerToPlonkup !ac =
  let !n = value @n

      !xPub = acOutput (acContext ac)
      !pubInputConstraints = L.map var (toList xPub)
      !plonkConstraints =
        L.map
          (evalPolynomial evalMonomial (var . toVar))
          (elems (acSystem (acContext ac)))

      toTriple :: [t] -> (t, t, t)
      toTriple [!x] = (x, x, x)
      toTriple [!x, !y] = (x, x, y)
      toTriple [!x, !y, !z] = (x, y, z)
      toTriple ws = P.error ("Expected list of length 1-3, got " <> P.show (length ws))

      unfold :: LookupType a -> (Natural, (Vector a -> Vector a) -> [Vector a])
      unfold (LTRanges !rs) =
        let !segs = S.toList rs
         in ( sum [toConstant (hi - lo + one) | (lo, hi) <- segs]
            , pure . ($ V.concat [V.enumFromTo lo hi | (lo, hi) <- segs])
            )
      unfold (LTProduct !t !u) =
        let (!m, ts) = unfold t
            (!k, us) = unfold u
         in ( m * k
            , \f ->
                ts (f . V.concatMap (V.replicate (P.fromIntegral k)))
                  <> us (f . V.concat . L.replicate (P.fromIntegral m))
            )
      unfold (LTPlot !g !t) =
        let (!k, ts) = unfold t
            LookupFunction g' = acLookupFunction (acContext ac) ! g
         in ( k
            , \f ->
                let !ts' = ts id
                 in f <$> (ts' <> fmap (toVector k) (g' $ fromVector <$> ts'))
            )

      lkup
        :: LookupType a
        -> [[Var a]]
        -> ([LookupConstraint i a], Sum Natural, (Vector a, Vector a, Vector a))
      lkup !lt !vs =
        let (!k, !ts) = unfold lt
         in ( L.map (uncurry3 LookupConstraint . toTriple) vs
            , Sum k
            , toTriple (ts id)
            )

      (!xLookup, Sum !nLookup, (!xs, !ys, !zs)) =
        case M.assocs (acLookup $ acContext ac) of
          [] -> ([], 0, (V.empty, V.empty, V.empty))
          [(lt, vs)] -> lkup lt [L.map toVar v | v <- S.toList vs]
          asscs ->
            flip foldMap (L.zip [(0 :: Natural) ..] asscs) $
              \(fromConstant -> i, (lt, vs)) ->
                lkup
                  (LTRanges (S.singleton (i, i)) `LTProduct` lt)
                  [fromConstant i : L.map toVar v | v <- S.toList vs]

      !lookupTableRows = V.zipWith3 (,,) xs ys zs

      !t1 = toPolyVec xs
      !t2 = toPolyVec ys
      !t3 = toPolyVec zs

      !cNum = acSizeN ac + length (tabulate @o id) + length xLookup

      !plonkupSystem =
        fromList $
          L.concat
            [ L.map (ConsPlonk . toPlonkConstraint) (pubInputConstraints ++ plonkConstraints)
            , ConsLookup <$> xLookup
            , replicate (n -! cNum) ConsExtra
            ]

      !qM = toPolyVec $ fmap (qm . getPlonkConstraint) plonkupSystem
      !qL = toPolyVec $ fmap (ql . getPlonkConstraint) plonkupSystem
      !qR = toPolyVec $ fmap (qr . getPlonkConstraint) plonkupSystem
      !qO = toPolyVec $ fmap (qo . getPlonkConstraint) plonkupSystem
      !qC = toPolyVec $ fmap (qc . getPlonkConstraint) plonkupSystem
      !qK = toPolyVec $ fmap isLookupConstraint plonkupSystem

      !a = fmap getA plonkupSystem
      !b = fmap getB plonkupSystem
      !c = fmap getC plonkupSystem

      !sigma = fromCycles @(PlonkupPermutationSize n) $ mkIndexPartition $ V.concat [a, b, c]

      eval = evalVar . witnessGenerator ac
      w1 i = toPolyVec $ fmap (eval i) a
      w2 i = toPolyVec $ fmap (eval i) b
      w3 i = toPolyVec $ fmap (eval i) c
      witness i = (w1 i, w2 i, w3 i)
      pubInput i = P.map (eval i) $ toList xPub

      prvNum = 0

      relation = PlonkupRelation {qM, qL, qR, qO, qC, qK, t1, t2, t3, sigma, witness, pubInput, prvNum}
   in if P.max cNum nLookup P.<= n
        then Just PlonkupLowering {relation, wireA = a, wireB = b, wireC = c, lookupTableRows}
        else Nothing

exportHalo2Ir
  :: forall i o n a pv
   . ( KnownNat n
     , Arithmetic a
     , Eq a
     , Binary a
     , Binary (Rep i)
     , UnivariateRingPolyVec a pv
     , Representable i
     , Representable o
     , Foldable o
     , Halo2FieldExport a
     )
  => ArithmeticCircuit a i o
  -> i a
  -> Either ExportError Halo2CircuitIR
exportHalo2Ir !ac !input = do
  PlonkupLowering {relation = rel, wireA, wireB, wireC, lookupTableRows} <-
    maybe (Left RelationTooSmall) Right (lowerToPlonkup @i @o @n @a @pv ac)

  let (!w1, !w2, !w3) = witness rel input
      !pubs = publicInputsText (pubInput rel input)

      rowCount :: P.Int
      !rowCount = P.fromIntegral (value @n)

      !zeroA = zero :: a
      !qMVec = fromPolyVec (qM rel)
      !qLVec = fromPolyVec (qL rel)
      !qRVec = fromPolyVec (qR rel)
      !qOVec = fromPolyVec (qO rel)
      !qCVec = fromPolyVec (qC rel)
      !qKVec = fromPolyVec (qK rel)
      !w1Vec = fromPolyVec w1
      !w2Vec = fromPolyVec w2
      !w3Vec = fromPolyVec w3

      atOr :: a -> P.Int -> Vector a -> a
      atOr !def !ix !v
        | ix P.< V.length v = v V.! ix
        | P.otherwise = def

      cellOf :: Var a -> a -> AssignedCell
      cellOf !varA !valA =
        AssignedCell
          { cellValue = halo2FieldText valA
          , equalityKey = equalityKeyOf varA
          }

      buildRow :: P.Int -> ImportedRow
      buildRow !ix =
        let !aVal = atOr zeroA ix w1Vec
            !bVal = atOr zeroA ix w2Vec
            !cVal = atOr zeroA ix w3Vec
            !instIx =
              if ix P.< P.length pubs
                then Just (P.fromIntegral ix)
                else Nothing
         in ImportedRow
              { rowIndex = P.fromIntegral ix
              , aCell = cellOf (wireA V.! ix) aVal
              , bCell = cellOf (wireB V.! ix) bVal
              , cCell = cellOf (wireC V.! ix) cVal
              , qMRow = halo2FieldText (atOr zeroA ix qMVec)
              , qLRow = halo2FieldText (atOr zeroA ix qLVec)
              , qRRow = halo2FieldText (atOr zeroA ix qRVec)
              , qORow = halo2FieldText (atOr zeroA ix qOVec)
              , qCRow = halo2FieldText (atOr zeroA ix qCVec)
              , qLookup = atOr zeroA ix qKVec /= zeroA
              , instanceIndex = instIx
              }

      !zeroText = halo2FieldText zeroA
      !lookupRows =
        LookupTableRow zeroText zeroText zeroText
          : fmap
            (\(x, y, z) -> LookupTableRow (halo2FieldText x) (halo2FieldText y) (halo2FieldText z))
            (V.toList lookupTableRows)
   in Right
        Halo2CircuitIR
          { formatVersion = 1
          , fieldTag = halo2FieldTag (P.undefined :: proxy a)
          , rows = fmap buildRow [0 .. rowCount P.- 1]
          , lookupTable = lookupRows
          , publicInputs = pubs
          }

publicInputsText :: Halo2FieldExport a => [a] -> [T.Text]
publicInputsText = fmap halo2FieldText

equalityKeyOf :: Binary a => Var a -> Maybe T.Text
equalityKeyOf (ConstVar _) = Nothing
equalityKeyOf v = Just ("bin:" <> hexLazy (encode v))

hexLazy :: LBS.ByteString -> T.Text
hexLazy = T.pack . LBS.foldr step []
 where
  step :: Word8 -> P.String -> P.String
  step w acc =
    let s = showHex w ""
     in (if P.length s == 1 then '0' : s else s) <> acc
