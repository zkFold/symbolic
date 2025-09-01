{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{- HLINT ignore "Use zipWithM" -}

module ZkFold.Symbolic.Data.List where

import Control.Applicative (pure)
import Control.Monad (return, sequence_)
import Data.Function (flip, id, ($), (.))
import Data.Functor (Functor (..), (<$>))
import Data.Functor.Rep (Representable (..), pureRep)
import qualified Data.List as Haskell
import Data.List.Infinite (Infinite (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Semialign (alignWith)
import Data.These (These (..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst, snd)
import Data.Type.Equality (type (~))
import GHC.Generics (Generic, Generic1, Par1 (..), U1 (..), (:*:) (..))

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (KnownNat)
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.HFunctor (hmap)
import ZkFold.Data.List.Infinite ()
import ZkFold.Data.Orphans ()
import ZkFold.Data.Product (fstP, sndP, uncurryP)
import qualified ZkFold.Symbolic.Algorithm.Interpolation as I
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Bool (Bool (..), BoolType (..), SymbolicEq)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))
import ZkFold.Symbolic.Data.Payloaded (Payloaded (..), payloaded)
import ZkFold.Symbolic.Data.UInt (UInt)
import ZkFold.Symbolic.Data.Vec (Vec (..))
import ZkFold.Symbolic.Fold
import ZkFold.Symbolic.MonadCircuit

newtype HashOf x c = HO {runHO :: c (Layout x (Order (BaseField c)))}

instance SymbolicData x => SymbolicData (HashOf x) where
  type Layout (HashOf x) k = Layout x k
  type Payload (HashOf x) _ = U1
  type HasRep (HashOf x) c = HasRep x c

  arithmetize = runHO
  payload _ = U1
  interpolate (fmap (runHO <$>) -> (bs :: NonEmpty (bc, c lx))) =
    HO . I.interpolate bs
  restore (l, _) = HO l

instance SymbolicData x => SymbolicInput (HashOf x) where
  isValid _ = true

emptyHash
  :: (Symbolic c, Representable (Layout x (Order (BaseField c)))) => HashOf x c
emptyHash = HO $ embed $ pureRep zero

deriving via
  (Vec (Layout x (Order (BaseField c))) c)
  instance
    (SymbolicData x, Symbolic c) => Eq (HashOf x c)

-- | TODO: Maybe some 'isValid' check for Lists?..
data List x c = List
  { lHash :: HashOf x c
  , lSize :: FieldElement c
  , lWitness :: Payloaded Infinite (HashOf x :*: x) c
  }
  deriving (Generic, Generic1, SymbolicData, SymbolicInput)

instance (SymbolicEq x c, Symbolic c) => Eq (List x c)

instance
  (FromConstant a (b c), SymbolicData b, HasRep b c, Symbolic c)
  => FromConstant [a] (List b c)
  where
  fromConstant = Haskell.foldr ((.:) . fromConstant) emptyList

size :: List x c -> FieldElement c
size = lSize

-- | TODO: A proof-of-concept where hash == id.
-- Replace id with a proper hash if we need lists to be cryptographically secure.
emptyList :: forall c x. (SymbolicData x, HasRep x c, Symbolic c) => List x c
emptyList = dummy

null :: forall c x. Symbolic c => List x c -> Bool c
null List {..} = lSize == zero

infixr 5 .:

(.:) :: forall c x. (SymbolicData x, Symbolic c) => x c -> List x c -> List x c
x .: List {..} =
  List
    { lSize = incSize
    , lHash = HO $ fromCircuit3F
        (runHO lHash)
        (arithmetize x)
        (fromFieldElement incSize)
        \vHash vRepr (Par1 s) -> sequence $ alignWith (hashFun s) vHash vRepr
    , lWitness =
        Payloaded $
          unPar1 (runPayloaded $ payloaded (Par1 $ lHash :*: x))
            :< runPayloaded lWitness
    }
 where
  incSize = lSize + one

hashFun :: MonadCircuit i a w m => i -> These i i -> m i
hashFun s (These h t) = newAssigned \x -> x h + x t * x s
hashFun _ (This h) = pure h
hashFun s (That t) = newAssigned \x -> x t * x s

-- | TODO: return symbolic Maybe
uncons :: forall x c. (SymbolicData x, Symbolic c) => List x c -> (x c, List x c)
uncons List {..} = case lWitness of
  Payloaded ((tailHash :*: headLayout, _ :*: headPayload) :< oldTail) ->
    ( restore (hmap fstP preimage, headPayload)
    , List (HO $ hmap sndP preimage) decSize $ Payloaded oldTail
    )
   where
    decSize = lSize - one
    preimage :: c (Layout x (Order (BaseField c)) :*: Layout x (Order (BaseField c)))
    preimage = fromCircuit2F
      (fromFieldElement decSize)
      (runHO lHash)
      \(Par1 s) y -> do
        tH :*: hH <- traverse unconstrained (tailHash :*: headLayout)
        hash <- sequence $ alignWith (hashFun s) hH tH
        sequence_ $ alignWith forceEq hash y
        return (hH :*: tH)
    forceEq (These i j) = constraint \x -> x i - x j
    forceEq (This i) = constraint \x -> x i
    forceEq (That j) = constraint \x -> x j

head :: (SymbolicData x, Symbolic c) => List x c -> x c
head = fst . uncons

tail :: (SymbolicData x, Symbolic c) => List x c -> List x c
tail = snd . uncons

foldl
  :: forall x y c
   . (SymbolicData x, HasRep x c, SymbolicData y, HasRep y c, SymbolicFold c)
  => (forall d. (SymbolicFold d, BaseField d ~ BaseField c) => y d -> x d -> y d)
  -> y c
  -> List x c
  -> y c
foldl f y List {..} =
  restore $
    sfoldl
      foldOp
      (arithmetize y)
      (payload y)
      (runHO lHash)
      ((\(_ :*: l, _ :*: p) -> l :*: p) <$> runPayloaded lWitness)
      (fromFieldElement lSize)
 where
  foldOp
    :: forall s n
     . ( SymbolicFold s
       , BaseField s ~ BaseField c
       , n ~ Order (BaseField s)
       , Functor (Payload x n)
       )
    => s (Layout y n)
    -> Payload y n (WitnessField s)
    -> s (Layout x n :*: Payload x n)
    -> (s (Layout y n), Payload y n (WitnessField s))
  foldOp yl yp il =
    let r = restore (yl, yp) `f` restore (hmap fstP il, witnessF (hmap sndP il))
     in (arithmetize r, payload r)

scanl
  :: forall c x y
   . (SymbolicData x, HasRep x c, SymbolicData y, HasRep y c, SymbolicFold c)
  => (forall d. SymbolicFold d => y d -> x d -> y d)
  -> y c
  -> List x c
  -> List y c
scanl f s =
  reverse
    . uncurryP (.:)
    . foldl (\(y :*: ys) x -> f y x :*: (y .: ys)) (s :*: emptyList)

-- | revapp xs ys = reverse xs ++ ys
revapp
  :: forall c x
   . (SymbolicData x, HasRep x c, SymbolicFold c)
  => List x c -> List x c -> List x c
revapp xs ys = foldl (flip (.:)) ys xs

reverse :: (SymbolicData x, HasRep x c, SymbolicFold c) => List x c -> List x c
reverse xs = revapp xs emptyList

last :: (SymbolicData x, HasRep x c, SymbolicFold c) => List x c -> x c
last = head . reverse

init :: (SymbolicData x, HasRep x c, SymbolicFold c) => List x c -> List x c
init = reverse . tail . reverse

(++)
  :: (SymbolicData x, HasRep x c, SymbolicFold c)
  => List x c -> List x c -> List x c
xs ++ ys = revapp (reverse xs) ys

foldr
  :: forall c x y
   . (SymbolicData x, HasRep x c, SymbolicData y, HasRep y c, SymbolicFold c)
  => (forall d. (SymbolicFold d, BaseField d ~ BaseField c) => x d -> y d -> y d)
  -> y c
  -> List x c
  -> y c
foldr f s xs = foldl (flip f) s (reverse xs)

mapWithCtx
  :: forall c g x y
   . ( SymbolicData g
     , HasRep g c
     , SymbolicData x
     , HasRep x c
     , SymbolicData y
     , HasRep y c
     , SymbolicFold c
     )
  => g c
  -> (forall d. SymbolicFold d => g d -> x d -> y d)
  -> List x c
  -> List y c
mapWithCtx g f =
  sndP . foldr (\x (g' :*: ys) -> g' :*: (f g' x .: ys)) (g :*: emptyList)

filter
  :: forall c x
   . (SymbolicData x, HasRep x c, SymbolicFold c)
  => (forall d. SymbolicFold d => x d -> Bool d)
  -> List x c
  -> List x c
filter pred = foldr (\x ys -> ifThenElse (pred x) (x .: ys) ys) emptyList

delete
  :: forall c x
   . (SymbolicData x, HasRep x c, SymbolicFold c)
  => (forall d. (SymbolicFold d, BaseField d ~ BaseField c) => x d -> x d -> Bool d)
  -> x c
  -> List x c
  -> List x c
delete eq x xs =
  let (_ :*: _ :*: result) =
        foldr
          ( \y (ok :*: y0 :*: ys) ->
              let test = eq y y0
               in (ok || test)
                    :*: y0
                    :*: ifThenElse (ok || not test) (y .: ys) ys
          )
          (false :*: x :*: emptyList)
          xs
   in result

class HasRep x c => HasRep' x c

instance HasRep x c => HasRep' x c

setminus
  :: forall c x
   . ( SymbolicData x
     , SymbolicFold c
     , forall d. (Symbolic d, BaseField d ~ BaseField c) => HasRep' x d
     )
  => (forall d. SymbolicFold d => x d -> x d -> Bool d)
  -> List x c
  -> List x c
  -> List x c
setminus f = foldl $ flip (delete f)

singleton
  :: forall c x. (SymbolicData x, HasRep x c, Symbolic c) => x c -> List x c
singleton x = x .: emptyList

(!!)
  :: forall x c n
   . (SymbolicData x, HasRep x c, SymbolicFold c)
  => (KnownNat n, KnownRegisters c n Auto)
  => List x c
  -> UInt n Auto c
  -> x c
xs !! n =
  sndP $
    foldl
      (\(m :*: y) x -> (m - one) :*: ifThenElse (m == zero) x y)
      (n :*: restore (embed $ pureRep zero, pureRep zero))
      xs

concatMap
  :: forall c x y
   . ( SymbolicData x
     , HasRep x c
     , SymbolicData y
     , SymbolicFold c
     , forall d. BaseField c ~ BaseField d => HasRep' y d
     )
  => (forall d. (SymbolicFold d, BaseField c ~ BaseField d) => x d -> List y d)
  -> List x c
  -> List y c
concatMap f = reverse . foldl (\ys x -> revapp (f x) ys) emptyList

concat
  :: forall c x
   . ( SymbolicData x
     , SymbolicFold c
     , forall d. BaseField c ~ BaseField d => HasRep' x d
     )
  => List (List x) c -> List x c
concat = concatMap id

findIndex
  :: forall x c n
   . (SymbolicData x, HasRep x c, SymbolicFold c, KnownNat n)
  => KnownRegisters c n Auto
  => (forall d. SymbolicFold d => x d -> Bool d)
  -> List x c
  -> UInt n Auto c
findIndex p =
  sndP
    . foldl (\(m :*: y) x -> (m + one) :*: ifThenElse (p x) m y) (zero :*: zero)

insert
  :: forall x c n
   . ( SymbolicData x
     , HasRep x c
     , SymbolicFold c
     , KnownNat n
     , KnownRegisters c n Auto
     )
  => List x c
  -> UInt n Auto c
  -> x c
  -> List x c
insert xs n xi =
  let _ :*: _ :*: res =
        foldr
          ( \a (n' :*: xi' :*: l') ->
              (n' - one)
                :*: xi'
                :*: ifThenElse (n' == zero) (xi' .: l') (a .: l')
          )
          (n :*: xi :*: emptyList)
          xs
   in res

slice
  :: forall c x
   . (SymbolicData x, HasRep x c, SymbolicFold c)
  => FieldElement c -> FieldElement c -> List x c -> List x c
slice f t xs =
  let _ :*: _ :*: res =
        foldr
          ( \x (skipCnt :*: lenCnt :*: l) ->
              ifThenElse
                (skipCnt == zero)
                ( ifThenElse
                    (lenCnt == zero)
                    (zero :*: zero :*: l)
                    (zero :*: (lenCnt - one) :*: (x .: l))
                )
                ((skipCnt - one) :*: lenCnt :*: l)
          )
          (f :*: t :*: emptyList)
          xs
   in res
