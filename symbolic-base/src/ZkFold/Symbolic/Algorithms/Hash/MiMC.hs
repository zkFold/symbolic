{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Algorithms.Hash.MiMC where

import           Data.Foldable                                  (toList)
import           Data.Functor.Rep                               (fmapRep, liftR3, pureRep)
import           Data.List.NonEmpty                             (NonEmpty ((:|)), nonEmpty)
import           Data.Proxy                                     (Proxy (..))
import           GHC.Generics                                   (Par1 (..), (:*:) (..))
import           Numeric.Natural                                (Natural)
import           Prelude                                        hiding (Eq (..), Num (..), any, length, not, (!!), (/),
                                                                 (^), (||))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Control.HApplicative               (hpair)
import           ZkFold.Base.Data.HFunctor                      (hmap)
import           ZkFold.Base.Data.Package                       (unpacked)
import           ZkFold.Symbolic.Algorithms.Hash.MiMC.Constants (mimcConstants)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.FieldElement
import           ZkFold.Symbolic.Interpreter
import           ZkFold.Symbolic.MonadCircuit                   (MonadCircuit (newAssigned))

-- | MiMC-2n/n (Feistel) hash function.
-- See https://eprint.iacr.org/2016/492.pdf, page 5
mimcHash2 :: forall c x a.(FromConstant a x, c ~ Context x, Symbolic c, SymbolicData x) => [a] -> a -> x -> x -> x
mimcHash2 (map fromConstant -> xs) (fromConstant @a @x -> k) =
  case nonEmpty (reverse xs) of
    Just cs -> go cs
    Nothing -> error "mimcHash: empty list"
  where
    go :: NonEmpty x -> x -> x -> x
    go (c :| cs) xL xR =
      let
        t5 = restore $ \s ->
          (fromCircuit3F
              (hpair (arithmetize xL s) (arithmetize k s)) (arithmetize c s) (arithmetize xR s)
              (\ (a1 :*: a2) a3 r
                  -> do s3 <- sequenceA (liftR3 (\p h t -> newAssigned (($ h) + ($ t) + ($ p))) a1 a2 a3)
                        p5 <- sequenceA (fmapRep (\ss -> newAssigned (($ ss) ^ (5 :: Natural))) s3)
                        mzipWithMRep (\h t -> newAssigned (($ h) + ($ t))) p5 r )
          , pureRep zero)
      in case nonEmpty cs of
          Just cs' -> go cs' t5 xL
          Nothing  -> t5

mimcHashN :: forall a x. (FromConstant a x, Ring x, SymbolicData x) => [a] -> a -> [x] -> x
mimcHashN xs k = go
  where
    go zs = case zs of
      []          -> mimcHash2 xs k zero zero
      [z]         -> mimcHash2 xs k zero z
      [zL, zR]    -> mimcHash2 xs k zL zR
      (zL:zR:zs') -> go (mimcHash2 xs k zL zR : zs')

hash :: forall context x a .
    ( SymbolicOutput x
    , BaseField context ~ a
    , Context x ~ context
    ) => x -> FieldElement context
hash = mimcHashN mimcConstants (zero :: a) . fmap FieldElement . unpacked . hmap toList . flip arithmetize Proxy

mimcHashN' :: forall a x. (FromConstant a x, Arithmetic x) => [a] -> a -> [x] -> x
mimcHashN' xs k as = ans
  where
    FieldElement (Interpreter (Par1 ans)) = go (map (FieldElement .Interpreter . Par1) as)

    go :: [FieldElement (Interpreter x)] -> FieldElement (Interpreter x)
    go zs = case  zs of
      []          -> mimcHash2 xs k zero zero
      [z]         -> mimcHash2 xs k zero z
      [zL, zR]    -> mimcHash2 xs k zL zR
      (zL:zR:zs') -> go (mimcHash2 xs k zL zR : zs')
