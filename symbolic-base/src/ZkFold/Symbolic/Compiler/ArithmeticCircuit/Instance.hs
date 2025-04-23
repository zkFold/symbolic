{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans     #-}

module ZkFold.Symbolic.Compiler.ArithmeticCircuit.Instance () where

import           Control.DeepSeq                                     (NFData)
import           Control.Monad                                       (foldM)
import           Data.Aeson                                          hiding (Bool)
import           Data.Binary                                         (Binary)
import           Data.Foldable                                       (Foldable)
import           Data.Functor.Rep                                    (Representable (..))
import           Data.Map                                            hiding (drop, foldl, foldl', foldr, map, null,
                                                                      splitAt, take, toList)
import           Data.Ord                                            (Ord)
import           GHC.Generics                                        (Par1 (..), U1)
import           Prelude                                             (mempty, pure, return, ($), (<$>))
import qualified Prelude                                             as Haskell
import           Test.QuickCheck                                     (Arbitrary (arbitrary), Gen, elements)

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Number
import           ZkFold.Data.Vector                                  (Vector, unsafeToVector)
import           ZkFold.Prelude                                      (chooseVector)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Lookup   (LookupType)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var
import           ZkFold.Symbolic.MonadCircuit
import Data.Function (const, (.))

------------------------------------- Instances -------------------------------------

type ArbitraryConstraints a i =
  ( Arbitrary a
  , Arithmetic a
  , Binary a
  , Representable i
  , Foldable i
  , Binary (Rep i)
  , Ord (Rep i)
  , NFData (Rep i)
  )

instance
  ArbitraryConstraints a i => Arbitrary (ArithmeticCircuit a i U1) where
    arbitrary = foldM (\acc _ -> arbitraryConstraint acc) mempty [1:: Natural .. 10]

instance
  ArbitraryConstraints a i => Arbitrary (ArithmeticCircuit a i Par1) where
    arbitrary = do
      ac  <- arbitrary @(ArithmeticCircuit a i U1)
      out <- Par1 . toVar @a <$> elements (getAllVars ac)
      return $ ac `crown` out

instance
  (ArbitraryConstraints a i, KnownNat l) => Arbitrary (ArithmeticCircuit a i (Vector l)) where
    arbitrary = do
        ac  <- arbitrary @(ArithmeticCircuit a i U1)
        out <- tabulate . const . toVar <$> elements (getAllVars ac)
        return $ ac `crown` out

arbitraryConstraint :: forall a i o . ArbitraryConstraints a i
  => ArithmeticCircuit a i o -> Gen (ArithmeticCircuit a i o)
arbitraryConstraint ac = do
  con <- elements [arbitraryPolynomialConstraint, arbitraryLookupConstraint]
  con ac

arbitraryPolynomialConstraint :: forall a i o . ArbitraryConstraints a i
  => ArithmeticCircuit a i o -> Gen (ArithmeticCircuit a i o)
arbitraryPolynomialConstraint ac = do
  qm <- arbitrary :: Gen a
  ql <- arbitrary :: Gen a
  qr <- arbitrary :: Gen a
  qo <- arbitrary :: Gen a
  qc <- arbitrary :: Gen a
  let solve :: forall var w m . MonadCircuit var a w m => var -> var -> m var
      solve l r = do
        let p :: ClosedPoly var a
            p = \x -> scale qm one * x l * x r + scale ql one * x l + scale qr one * x r + scale qc one
        newConstrained (\x o -> p x + scale qo one * x o) (negate $ p at // fromConstant qo)
  let vars = getAllVars ac
  l <- toVar @a <$> elements vars
  r <- toVar @a <$> elements vars
  return $ fromCircuit3F (crown ac $ Par1 l) (crown ac $ Par1 r) ac $ \(Par1 i) (Par1 j) k -> do
    _ <- solve i j
    return k

arbitraryLookupConstraint :: forall a i o . ArbitraryConstraints a i
  => ArithmeticCircuit a i o -> Gen (ArithmeticCircuit a i o)
arbitraryLookupConstraint ac = do
  let solve :: forall var w m . MonadCircuit var a w m => var -> a -> m var
      solve v b = do
        let w :: w
            w = fromIntegral $ toIntegral (at v :: w) `mod` (toIntegral $ fromConstant @_ @w (b + one))
        newRanged b w
  let vars = getAllVars ac
  v <- toVar @a <$> elements vars
  return $ fromCircuit2F (crown ac $ Par1 v) ac $ \(Par1 i) k -> do
    _ <- solve i (fromConstant (8 :: Natural))
    return k

-- TODO: add witness generation info to the JSON object
instance (ToJSON a, ToJSON (o (Var a i)), ToJSONKey a, FromJSONKey (Var a i), ToJSON (Rep i), ToJSON (LookupType a), ToJSONKey (LookupType a))
  => ToJSON (ArithmeticCircuit a i o) where
    toJSON r = object
        [
            "system" .= acSystem r,
            "lookup" .= acLookup r,
            "output" .= acOutput r
        ]

-- TODO: properly restore the witness generation function
instance (FromJSON a, FromJSON (o (Var a i)), ToJSONKey (Var a i), FromJSONKey a, Haskell.Ord a, Haskell.Ord (Rep i), FromJSON (Rep i))
  => FromJSON (ArithmeticCircuit a i o) where
    parseJSON =
        withObject "ArithmeticCircuit" $ \v -> do
            acSystem   <- v .: "system"
            acLookup   <- v .: "lookup"
            acOutput   <- v .: "output"
            let acWitness        = empty
                acFold           = empty
                acLookupFunction = empty
            pure ArithmeticCircuit{..}
