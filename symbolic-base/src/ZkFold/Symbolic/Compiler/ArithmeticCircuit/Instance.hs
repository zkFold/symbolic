{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans     #-}

module ZkFold.Symbolic.Compiler.ArithmeticCircuit.Instance where

import           Control.DeepSeq                                     (NFData)
import           Data.Aeson                                          hiding (Bool)
import           Data.Binary                                         (Binary)
import           Data.Bool                                           (bool)
import           Data.Functor.Rep                                    (Representable (..))
import           Data.Map                                            hiding (drop, foldl, foldl', foldr, map, null,
                                                                      splitAt, take, toList)
import           GHC.Generics                                        (Par1 (..))
import           Prelude                                             (Show, head, mempty, pure, return, show, ($), (++),
                                                                      (.), (<$>), (<))
import qualified Prelude                                             as Haskell
import           Test.QuickCheck                                     (Arbitrary (arbitrary), Gen, elements)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Data.Vector                             (Vector, unsafeToVector)
import           ZkFold.Prelude                                      (chooseFromList, length)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Lookup   (LookupType)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var
import           ZkFold.Symbolic.Data.FieldElement                   (FieldElement (..))
import           ZkFold.Symbolic.MonadCircuit

------------------------------------- Instances -------------------------------------

instance
  ( Arithmetic a
  , Arbitrary a
  , Binary a
  , Binary (Rep p)
  , Arbitrary (Rep i)
  , Binary (Rep i)
  , Haskell.Ord (Rep i)
  , NFData (Rep i)
  , Representable i
  , Haskell.Foldable i
  ) => Arbitrary (ArithmeticCircuit a p i Par1) where
    arbitrary = do
        outVar <- toVar . InVar <$> arbitrary
        let ac = mempty {acOutput = Par1 outVar}
        fromFieldElement <$> arbitrary' (FieldElement ac) 10

instance
  ( Arithmetic a
  , Arbitrary a
  , Binary a
  , Binary (Rep p)
  , Arbitrary (Rep i)
  , Binary (Rep i)
  , Haskell.Ord (Rep i)
  , NFData (Rep i)
  , Representable i
  , Haskell.Foldable i
  , KnownNat l
  ) => Arbitrary (ArithmeticCircuit a p i (Vector l)) where
    arbitrary = do
        ac <- arbitrary @(ArithmeticCircuit a p i Par1)
        o  <- unsafeToVector <$> chooseFromList (value @l) (getAllVars ac)
        return ac {acOutput = toVar <$> o}

arbitrary' ::
  forall a p i .
  (Arithmetic a, Binary a, Binary (Rep p), Binary (Rep i), Haskell.Ord (Rep i), NFData (Rep i)) =>
  (Representable i, Haskell.Foldable i) =>
  FieldElement (ArithmeticCircuit a p i) -> Natural ->
  Gen (FieldElement (ArithmeticCircuit a p i))
arbitrary' ac 0 = return $ bool ac (newF * newF) (numOfVars < 2)
  where
    vars = getAllVars $ fromFieldElement ac
    numOfVars = length vars
    newF = FieldElement (fromFieldElement ac) { acOutput = pure (toVar $ head vars)}
arbitrary' ac iter = do
    let vars = getAllVars (fromFieldElement ac)
    li <- elements vars
    ri <- elements vars
    let (l, r) = ( FieldElement (fromFieldElement ac) { acOutput = pure (toVar li)}
                 , FieldElement (fromFieldElement ac) { acOutput = pure (toVar ri)})
    let c = FieldElement (fromFieldElement $ createRangeConstraint ac (fromConstant @Natural 10)) { acOutput = pure (toVar li)}

    ac' <- elements [
        l + r
        , l * r
        , l - r
        , l // r
        , c
        ]
    arbitrary' ac' (iter -! 1)


createRangeConstraint :: Symbolic c => FieldElement c -> BaseField c -> FieldElement c
createRangeConstraint (FieldElement x) a = FieldElement $ fromCircuitF x (\ (Par1 v) ->  Par1 <$> solve v a)
  where
    solve :: MonadCircuit var a w m => var -> a -> m var
    solve v b = do
      v' <- newAssigned (Haskell.const zero)
      rangeConstraint v' b
      return v

-- TODO: make it more readable
instance (Show a, Show (o (Var a i)), Show (Var a i), Show (Rep i), Haskell.Ord (Rep i)) => Show (ArithmeticCircuit a p i o) where
    show r = "ArithmeticCircuit { acSystem = " ++ show (acSystem r)
                          ++ "\n, acRange = " ++ show (acLookup r)
                          ++ "\n, acOutput = " ++ show (acOutput r)
                          ++ " }"

-- TODO: add witness generation info to the JSON object
instance (ToJSON a, ToJSON (o (Var a i)), ToJSONKey a, FromJSONKey (Var a i), ToJSON (Rep i), ToJSON (LookupType a), ToJSONKey (LookupType a)) => ToJSON (ArithmeticCircuit a p i o) where
    toJSON r = object
        [
            "system" .= acSystem r,
            "lookup" .= acLookup r,
            "output" .= acOutput r
        ]

-- TODO: properly restore the witness generation function
instance (FromJSON a, FromJSON (o (Var a i)), ToJSONKey (Var a i), FromJSONKey a, Haskell.Ord a, Haskell.Ord (Rep i), FromJSON (Rep i)) => FromJSON (ArithmeticCircuit a p i o) where
    parseJSON =
        withObject "ArithmeticCircuit" $ \v -> do
            acSystem   <- v .: "system"
            acLookup   <- v .: "lookup"
            acOutput   <- v .: "output"
            let acWitness        = empty
                acFold           = empty
                acLookupFunction = empty
            pure ArithmeticCircuit{..}
