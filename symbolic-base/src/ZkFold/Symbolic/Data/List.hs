{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Data.List where

import Control.Monad (return)
import Data.Distributive (Distributive (..))
import Data.Function (const, ($), (.))
import Data.Functor (Functor (..), (<$>))
import Data.Functor.Rep (Representable (..), pureRep, tabulate)
import qualified Data.List as Haskell
import Data.List.Infinite (Infinite (..))
import Data.Traversable (traverse)
import Data.Tuple (fst, snd, uncurry)
import Data.Type.Equality (type (~))
import GHC.Generics (Generic, Generic1, Par1 (..), (:*:) (..), (:.:) (..))

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (KnownNat)
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.HFunctor (hmap)
import ZkFold.Data.List.Infinite ()
import ZkFold.Data.Orphans ()
import ZkFold.Data.Product (fstP, sndP)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Bool (Bool (..), BoolType (..), SymbolicEq)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))
import ZkFold.Symbolic.Data.Payloaded (Payloaded (Payloaded, runPayloaded))
import ZkFold.Symbolic.Data.UInt (UInt)
import ZkFold.Symbolic.Fold
import ZkFold.Symbolic.MonadCircuit

newtype HashOf x c = HO { runHO :: c (Layout x (Order (BaseField c))) }

data List x c = List
  { lHash :: HashOf x c
  , lSize :: FieldElement c
  , lWitness :: Payloaded (Infinite :.: (HashOf x :*: x)) c
  }
  deriving Generic

instance SymbolicData x => SymbolicData (List x)

-- | TODO: Maybe some 'isValid' check for Lists?..
instance SymbolicData x => SymbolicInput (List x)

instance (SymbolicData x, SymbolicEq x, Symbolic c) => Eq (List x c)

instance
  (FromConstant a (b c), SymbolicData b, Symbolic c) => FromConstant [a] (List b c) where
  fromConstant = Haskell.foldr ((.:) . fromConstant) emptyList

size :: List c x -> FieldElement c
size = FieldElement . lSize

-- | TODO: A proof-of-concept where hash == id.
-- Replace id with a proper hash if we need lists to be cryptographically secure.
emptyList :: forall c x . (SymbolicData x, Symbolic c) => List x c
emptyList =
  List (embed $ pureRep zero) (fromFieldElement zero) $
    Payloaded $
      tabulate (const zero)

null :: forall c x . Symbolic c => List x c -> Bool c
null List {..} = FieldElement lSize == zero

infixr 5 .:

(.:) :: forall c x . (SymbolicData x, Symbolic c) => x c -> List x c -> List x c
x .: List {..} = List incHash incSize incWitness
 where
  headL = arithmetize x
  headP = payload x
  incHash = fromCircuit3F lHash headL incSize \vHash vRepr (Par1 s) ->
    mzipWithMRep (hashFun s) vHash vRepr
  incSize = fromFieldElement (FieldElement lSize + one)
  incItem = ListItem (witnessF lHash) (witnessF headL) headP
  Payloaded (Comp1 srcWitness) = lWitness
  incWitness = Payloaded $ Comp1 (incItem :< srcWitness)

hashFun :: MonadCircuit i a w m => i -> i -> i -> m i
hashFun s h t = newAssigned (($ h) + ($ t) * ($ s))

-- | TODO: Is there really a nicer way to handle empty lists?
uncons ::
  forall x c . (SymbolicData x, Symbolic c) => List x c -> (x c, List x c)
uncons List {..} = case lWitness of
  Payloaded (Comp1 (ListItem {..} :< tWitness)) ->
    ( restore (hmap fstP preimage, headPayload)
    , List (hmap sndP preimage) decSize $ Payloaded (Comp1 tWitness)
    )
   where
    decSize = fromFieldElement (FieldElement lSize - one)

    preimage :: c (Layout x :*: Layout x)
    preimage = fromCircuit2F decSize lHash $ \(Par1 s) y -> do
      tH :*: hH <- traverse unconstrained (tailHash :*: headLayout)
      hash <- mzipWithMRep (hashFun s) hH tH
      _ <- mzipWithMRep (\i j -> constraint (($ i) - ($ j))) hash y
      return (hH :*: tH)

head :: (SymbolicData x, Symbolic c) => List x c -> x c
head = fst . uncons

tail :: (SymbolicData x, Symbolic c) => List x c -> List x c
tail = snd . uncons

foldl ::
  forall x y c . (SymbolicData x, SymbolicData y, SymbolicFold c) =>
  (forall d. SymbolicFold d => y d -> x d -> y d) -> y c -> List x c -> y c
foldl f y List {..} =
  restore $
    sfoldl
      foldOp
      (arithmetize y)
      (payload y)
      lHash
      ( fmap (\ListItem {..} -> headLayout :*: headPayload)
          . unComp1
          $ runPayloaded lWitness
      )
      lSize
 where
  foldOp
    :: forall s n
     . (SymbolicFold s, BaseField s ~ BaseField c, n ~ Order (BaseField s))
    => s (Layout y n)
    -> Payload y n (WitnessField s)
    -> s (Layout x n :*: Payload x n)
    -> (s (Layout y n), Payload y n (WitnessField s))
  foldOp yl yp il = _

scanl ::
  forall c x y . (SymbolicFold c, SymbolicData x, SymbolicData y) =>
  (forall d. SymbolicFold d => y d -> x d -> y d) -> y c -> List x c -> List y c
scanl f s =
  reverse . uncurry (.:) . foldl (\(y, ys) x -> (f y x, y .: ys)) (s, emptyList)

-- | revapp xs ys = reverse xs ++ ys
revapp ::
  forall c x . (SymbolicData x, SymbolicFold c) =>
  List x c -> List x c -> List x c
revapp xs ys = foldl (flip (.:)) ys xs

reverse :: (SymbolicData x, SymbolicFold c) => List x c -> List x c
reverse xs = revapp xs emptyList

last :: (SymbolicData x, SymbolicFold c) => List x c -> x c
last = head . reverse

init :: (SymbolicData x, SymbolicFold c) => List x c -> List x c
init = reverse . tail . reverse

(++) :: (SymbolicData x, SymbolicFold c) => List x c -> List x c -> List x c
xs ++ ys = revapp (reverse xs) ys

foldr
  :: forall c x y . (SymbolicData x, SymbolicFold c, SymbolicData y) =>
  (forall d. SymbolicFold d => x d -> y d -> y d) -> y c -> List x c -> y c
foldr f s xs = foldl (flip f) s (reverse xs)

mapWithCtx
  :: forall c g x y
   . ( SymbolicFold c
     , SymbolicData g
     , SymbolicData x
     , SymbolicData y
     )
  => g c
  -> (forall d. SymbolicFold d => g d -> x d -> y d)
  -> List x c
  -> List y c
mapWithCtx g f = snd . foldr (\x (g', ys) -> (g', f g' x .: ys)) (g, emptyList)

filter
  :: forall c x
   . (SymbolicData x, SymbolicFold c)
  => (forall d. SymbolicFold d => x d -> Bool d)
  -> List x c
  -> List x c
filter pred = foldr (\(x, ys) -> ifThenElse (pred x) (x .: ys) ys) emptyList

delete
  :: forall c x
   . (SymbolicData x, SymbolicFold c)
  => (forall d. SymbolicFold d => x d -> x d -> Bool d)
  -> x c
  -> List x c
  -> List x c
delete eq x xs =
  let (_, _, result) =
        foldr
          (\y (ok, y0, ys) ->
              let test = eq y y0
               in (ok || test, y0, ifThenElse (ok || not test) (y .: ys) ys)
          )
          (false, x, emptyList)
          xs
   in result

setminus
  :: forall c x
   . (SymbolicData x, SymbolicFold c)
  => (forall d. SymbolicFold d => x d -> x d -> Bool d)
  -> List x c
  -> List x c
  -> List x c
setminus = foldl . flip . delete

singleton :: forall c x . (SymbolicData x, Symbolic c) => x c -> List x c
singleton x = x .: emptyList

(!!)
  :: forall x c n
   . (SymbolicData x, SymbolicFold c)
  => (KnownNat n, KnownRegisters c n Auto)
  => List x c
  -> UInt n Auto c
  -> x c
xs !! n =
  snd $
    foldl
      (\(m, y) x -> (m - one, ifThenElse (m == zero) x y))
      (n, restore (embed $ pureRep zero, pureRep zero))
      xs

concatMap
  :: forall c x y
   . (SymbolicFold c, SymbolicData x, SymbolicData y)
  => (forall d. SymbolicFold d => x d -> List y d) -> List x c -> List y c
concatMap f = reverse . foldl (\ys x -> revapp (f x) ys) emptyList

concat
  :: forall c x . (SymbolicData x, SymbolicFold c)
  => List (List x) c -> List x c
concat = concatMap id

findIndex
  :: forall x c n
   . (SymbolicData x, SymbolicFold c, KnownNat n, KnownRegisters c n Auto)
  => (forall d. SymbolicFold d => x d -> Bool d)
  -> List x c
  -> UInt n Auto c
findIndex p =
  snd . foldl (\(m, y) x -> (m + one, ifThenElse (p x) m y)) (zero, zero)

insert
  :: forall x c n
   . (SymbolicData x, SymbolicFold c, KnownNat n, KnownRegisters c n Auto)
  => List x c
  -> UInt n Auto c
  -> x c
  -> List x c
insert xs n xi =
  let (_, _, res) =
        foldr
          (\a (n', xi', l') ->
              (n' - one, xi', ifThenElse (n' == zero) (xi' .: l') (a .: l'))
          )
          (n, xi, emptyList)
          xs
   in res

slice
  :: forall c x
   . (SymbolicFold c, SymbolicData x)
  => FieldElement c -> FieldElement c -> List x c -> List x c
slice f t xs =
  let (_, _, res) =
        foldr
          (\x (skipCnt, lenCnt, l) ->
                ifThenElse
                  (skipCnt == zero)
                  ( ifThenElse
                      (lenCnt == zero)
                      (zero, zero, l)
                      (zero, lenCnt - one, x .: l)
                  )
                  (skipCnt - one, lenCnt, l)
          )
          (f, t, emptyList)
          xs
   in res
