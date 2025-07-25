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
import ZkFold.Symbolic.Data.Morph (MorphFrom, MorphTo (..), (@))
import ZkFold.Symbolic.Data.Payloaded (Payloaded (Payloaded, runPayloaded))
import ZkFold.Symbolic.Data.Switch (Switch (..))
import ZkFold.Symbolic.Data.UInt (UInt)
import ZkFold.Symbolic.Fold
import ZkFold.Symbolic.MonadCircuit

data ListItem l p a = ListItem
  { tailHash :: l a
  , headLayout :: l a
  , headPayload :: p a
  }
  deriving (Functor, Generic1, Representable)

instance (Distributive l, Distributive p) => Distributive (ListItem l p) where
  distribute f =
    ListItem
      { tailHash = distribute (tailHash <$> f)
      , headLayout = distribute (headLayout <$> f)
      , headPayload = distribute (headPayload <$> f)
      }

data List c x = List
  { lHash :: c (Layout x)
  , lSize :: c Par1
  , lWitness :: Payloaded (Infinite :.: ListItem (Layout x) (Payload x)) c
  }
  deriving Generic

instance (SymbolicData x, c ~ Context x) => SymbolicData (List c x)

-- | TODO: Maybe some 'isValid' check for Lists?..
instance (SymbolicInput x, c ~ Context x) => SymbolicInput (List c x)

instance (SymbolicData x, SymbolicEq x, c ~ Context x) => Eq (List c x)

instance (FromConstant a b, SymbolicData b, Context b ~ c) => FromConstant [a] (List c b) where
  fromConstant = Haskell.foldr ((.:) . fromConstant) emptyList

size :: List c x -> FieldElement c
size = FieldElement . lSize

-- | TODO: A proof-of-concept where hash == id.
-- Replace id with a proper hash if we need lists to be cryptographically secure.
emptyList
  :: forall context x
   . SymbolicData x
  => Context x ~ context
  => List context x
emptyList =
  List (embed $ pureRep zero) (fromFieldElement zero) $
    Payloaded $
      tabulate (const zero)

null
  :: forall context x
   . Symbolic context
  => List context x
  -> Bool context
null List {..} = FieldElement lSize == zero

infixr 5 .:

(.:)
  :: forall context x
   . SymbolicData x
  => Context x ~ context
  => x
  -> List context x
  -> List context x
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
uncons
  :: forall c x
   . SymbolicData x
  => Context x ~ c
  => List c x
  -> (x, List c x)
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

head
  :: SymbolicData x
  => Context x ~ c
  => List c x
  -> x
head = fst . uncons

tail
  :: SymbolicData x
  => Context x ~ c
  => List c x
  -> List c x
tail = snd . uncons

foldl
  :: forall x y c
   . ( SymbolicData x
     , SymbolicData y
     , Context y ~ c
     , SymbolicFold c
     )
  => MorphFrom c (y, x) y
  -> y
  -> List c x
  -> y
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
    :: forall s
     . (SymbolicFold s, BaseField s ~ BaseField c)
    => s (Layout y)
    -> Payload y (WitnessField s)
    -> s (Layout x :*: Payload x)
    -> (s (Layout y), Payload y (WitnessField s))
  foldOp yl yp il =
    let Switch {..} :: Switch s y =
          f
            @ ( Switch yl yp :: Switch s y
              , Switch (hmap fstP il) (witnessF (hmap sndP il))
                  :: Switch s x
              )
     in (sLayout, sPayload)

scanl
  :: forall c x y
   . (SymbolicFold c, SymbolicData x, SymbolicData y, Context y ~ c)
  => MorphFrom c (y, x) y -> y -> List c x -> List c y
scanl f s =
  reverse
    . uncurry (.:)
    . foldl
      ( Morph \((y :: Switch s y, ys), x :: Switch s x) ->
          (f @ (y, x) :: Switch s y, y .: ys)
      )
      (s, emptyList)

-- | revapp xs ys = reverse xs ++ ys
revapp
  :: forall c x
   . (SymbolicData x, Context x ~ c, SymbolicFold c)
  => List c x
  -> List c x
  -> List c x
revapp xs ys = foldl (Morph \(zs, x :: Switch s x) -> x .: zs) ys xs

reverse
  :: (SymbolicData x, Context x ~ c, SymbolicFold c) => List c x -> List c x
reverse xs = revapp xs emptyList

last :: (SymbolicData x, Context x ~ c, SymbolicFold c) => List c x -> x
last = head . reverse

init
  :: (SymbolicData x, Context x ~ c, SymbolicFold c) => List c x -> List c x
init = reverse . tail . reverse

(++)
  :: (SymbolicData x, Context x ~ c, SymbolicFold c)
  => List c x
  -> List c x
  -> List c x
xs ++ ys = revapp (reverse xs) ys

foldr
  :: forall c x y
   . (SymbolicData x, Context x ~ c, SymbolicFold c)
  => (SymbolicData y, Context y ~ c)
  => MorphFrom c (x, y) y
  -> y
  -> List c x
  -> y
foldr f s xs =
  foldl
    ( Morph \(y :: Switch s y, x :: Switch s x) ->
        f @ (x, y) :: Switch s y
    )
    s
    (reverse xs)

mapWithCtx
  :: forall c g x y
   . ( SymbolicFold c
     , SymbolicData g
     , Context g ~ c
     , SymbolicData x
     , Context x ~ c
     , SymbolicData y
     , Context y ~ c
     )
  => g -> MorphFrom c (g, x) y -> List c x -> List c y
mapWithCtx g f =
  snd
    . foldr
      ( Morph \(x :: Switch s x, (g' :: Switch s g, ys)) ->
          (g', (f @ (g', x) :: Switch s y) .: ys)
      )
      (g, emptyList)

filter
  :: forall c x
   . (SymbolicData x, Context x ~ c, SymbolicFold c)
  => MorphFrom c x (Bool c)
  -> List c x
  -> List c x
filter pred =
  foldr
    ( Morph \(x :: Switch s x, ys) ->
        ifThenElse (pred @ x :: Bool s) (x .: ys) ys
    )
    emptyList

delete
  :: forall c x
   . (SymbolicData x, Context x ~ c, SymbolicFold c)
  => MorphFrom c (x, x) (Bool c)
  -> x
  -> List c x
  -> List c x
delete eq x xs =
  let (_, _, result) =
        foldr
          ( Morph \(y :: Switch s x, (ok :: Bool s, y0 :: Switch s x, ys)) ->
              let test = eq @ (y, y0)
               in (ok || test, y0, ifThenElse (ok || not test) (y .: ys) ys)
          )
          (false :: Bool c, x, emptyList)
          xs
   in result

setminus
  :: forall c x
   . (SymbolicData x, Context x ~ c, SymbolicFold c)
  => MorphFrom c (x, x) (Bool c)
  -> List c x
  -> List c x
  -> List c x
setminus eq = foldl $ Morph \(xs, x :: Switch s x) ->
  delete
    ( Morph \(y :: Switch s' x, z :: Switch s' x) ->
        eq @ (y, z) :: Bool s'
    )
    x
    xs

singleton
  :: forall context x
   . SymbolicData x
  => Context x ~ context
  => x
  -> List context x
singleton x = x .: emptyList

(!!)
  :: forall x c n
   . (SymbolicData x, Context x ~ c, SymbolicFold c)
  => (KnownNat n, KnownRegisters c n Auto)
  => List c x
  -> UInt n Auto c
  -> x
xs !! n =
  snd $
    foldl
      ( Morph \((m :: UInt n Auto s, y :: Switch s x), x) ->
          (m - one, ifThenElse (m == zero :: Bool s) x y)
      )
      (n, restore (embed $ pureRep zero, pureRep zero))
      xs

concatMap
  :: forall c x y
   . (SymbolicFold c, SymbolicData x, SymbolicData y, Context y ~ c)
  => MorphFrom c x (List c y) -> List c x -> List c y
concatMap f =
  reverse
    . foldl
      ( Morph
          \(ys :: List s (Switch s y), x :: Switch s x) -> revapp (f @ x) ys
      )
      emptyList

concat
  :: forall c x
   . (SymbolicData x, Context x ~ c, SymbolicFold c)
  => List c (List c x)
  -> List c x
concat = concatMap (Morph \(x :: List s (Switch s x)) -> x)

findIndex
  :: forall x c n
   . (SymbolicData x, Context x ~ c, SymbolicFold c)
  => ( KnownNat n
     , KnownRegisters c n Auto
     )
  => MorphFrom c x (Bool c)
  -> List c x
  -> UInt n Auto c
findIndex p xs =
  snd $
    foldl
      ( Morph \((m :: UInt n Auto s, y :: UInt n Auto s), x :: Switch s x) ->
          (m + one, ifThenElse (p @ x :: Bool s) m y)
      )
      (zero :: UInt n Auto c, zero)
      xs

insert
  :: forall x c n
   . (SymbolicData x, Context x ~ c, SymbolicFold c)
  => (KnownNat n, KnownRegisters c n Auto)
  => List c x
  -> UInt n Auto c
  -> x
  -> List c x
insert xs n xi =
  let (_, _, res) =
        foldr
          ( Morph \(a :: Switch s x, (n' :: UInt n Auto s, xi', l')) ->
              (n' - one, xi', ifThenElse (n' == zero :: Bool s) (xi' .: l') (a .: l'))
          )
          (n, xi, emptyList)
          xs
   in res

slice
  :: forall c x
   . (SymbolicFold c, SymbolicData x, Context x ~ c)
  => FieldElement c -> FieldElement c -> List c x -> List c x
slice f t xs =
  let (_, _, res) =
        foldr
          ( Morph
              \(x :: Switch s x, (skipCnt :: FieldElement s, lenCnt :: FieldElement s, l)) ->
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
