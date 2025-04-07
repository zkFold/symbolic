{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators  #-}

module ZkFold.Symbolic.Data.List where

import           Control.Monad                     (return)
import           Data.Distributive                 (Distributive (..))
import           Data.Function                     (const, ($), (.))
import           Data.Functor                      (Functor (..), (<$>))
import           Data.Functor.Rep                  (Representable (..), pureRep, tabulate)
import           Data.List.Infinite                (Infinite (..))
import           Data.Proxy                        (Proxy (..))
import           Data.Traversable                  (traverse)
import           Data.Tuple                        (fst, snd)
import           Data.Type.Equality                (type (~))
import           GHC.Generics                      (Generic, Generic1, Par1 (..), (:*:) (..), (:.:) (..))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number  (KnownNat)
import           ZkFold.Base.Data.HFunctor         (hmap)
import           ZkFold.Base.Data.List.Infinite    ()
import           ZkFold.Base.Data.Orphans          ()
import           ZkFold.Base.Data.Product          (fstP, sndP)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool         (Bool (..), BoolType (..))
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Conditional  (Conditional, ifThenElse)
import           ZkFold.Symbolic.Data.Eq           (Eq (..), SymbolicEq)
import           ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import           ZkFold.Symbolic.Data.Input        (SymbolicInput (..))
import           ZkFold.Symbolic.Data.Morph        (MorphFrom, MorphTo (..), (@))
import           ZkFold.Symbolic.Data.Payloaded    (Payloaded (Payloaded, runPayloaded))
import           ZkFold.Symbolic.Data.Switch       (Switch (..))
import           ZkFold.Symbolic.Data.UInt         (UInt)
import           ZkFold.Symbolic.Fold
import           ZkFold.Symbolic.MonadCircuit

data ListItem l p a = ListItem
  { tailHash    :: l a
  , headLayout  :: l a
  , headPayload :: p a
  }
  deriving (Functor, Generic1, Representable)

instance (Distributive l, Distributive p) => Distributive (ListItem l p) where
  distribute f = ListItem
    { tailHash = distribute (tailHash <$> f)
    , headLayout = distribute (headLayout <$> f)
    , headPayload = distribute (headPayload <$> f)
    }

data List c x = List
  { lHash    :: c (Layout x)
  , lSize    :: c Par1
  , lWitness :: Payloaded (Infinite :.: ListItem (Layout x) (Payload x)) c
  }
  deriving (Generic)

instance (SymbolicData x, c ~ Context x) => SymbolicData (List c x)
-- | TODO: Maybe some 'isValid' check for Lists?..
instance (SymbolicInput x, c ~ Context x) => SymbolicInput (List c x)
instance (SymbolicData x, c ~ Context x) => Conditional (Bool c) (List c x)
instance (SymbolicData x, SymbolicEq x, c ~ Context x) => Eq (List c x)

-- | TODO: A proof-of-concept where hash == id.
-- Replace id with a proper hash if we need lists to be cryptographically secure.
--
emptyList
    :: forall context x
    .  SymbolicData x
    => Context x ~ context
    => List context x
emptyList =
  List (embed $ pureRep zero) (fromFieldElement zero) $
    Payloaded $ tabulate (const zero)

null
    :: forall context x
    .  Symbolic context
    => List context x
    -> Bool context
null List{..} = FieldElement lSize == zero

infixr 5 .:
(.:)
    :: forall context x
    .  SymbolicOutput x
    => Context x ~ context
    => x
    -> List context x
    -> List context x
x .: List{..} = List incHash incSize incWitness
  where
    headL = arithmetize x Proxy
    headP = payload x Proxy
    incHash = fromCircuit3F lHash headL incSize \vHash vRepr (Par1 s) ->
      mzipWithMRep (hashFun s) vHash vRepr
    incSize = fromFieldElement (FieldElement lSize + one)
    incItem = ListItem (witnessF lHash) (witnessF headL) headP
    Payloaded (Comp1 srcWitness) = lWitness
    incWitness = Payloaded $ Comp1 (incItem :< srcWitness)

hashFun :: MonadCircuit i a w m => i -> i -> i -> m i
hashFun s h t = newAssigned (($ h) + ($ t) * ($ s))

-- | TODO: Is there really a nicer way to handle empty lists?
--
uncons ::
  forall c x.
  SymbolicOutput x =>
  Context x ~ c =>
  List c x -> (x, List c x)
uncons List{..} = case lWitness of
  Payloaded (Comp1 (ListItem {..} :< tWitness)) ->
    ( restore $ const (hmap fstP preimage, headPayload)
    , List (hmap sndP preimage) decSize $ Payloaded (Comp1 tWitness))
    where
      decSize = fromFieldElement (FieldElement lSize - one)

      preimage :: c (Layout x :*: Layout x)
      preimage = fromCircuit2F decSize lHash $ \(Par1 s) y -> do
        tH :*: hH <- traverse unconstrained (tailHash :*: headLayout)
        hash <- mzipWithMRep (hashFun s) hH tH
        _ <- mzipWithMRep (\i j -> constraint (($ i) - ($ j))) hash y
        return (hH :*: tH)

head ::
  SymbolicOutput x =>
  Context x ~ c =>
  List c x -> x
head = fst . uncons

tail ::
  SymbolicOutput x =>
  Context x ~ c =>
  List c x -> List c x
tail = snd . uncons

foldl ::
  forall x y c.
  ( SymbolicOutput x, Context x ~ c
  , SymbolicOutput y, Context y ~ c
  , SymbolicFold c
  ) =>
  MorphFrom c (y, x) y -> y -> List c x -> y
foldl f y List {..} = restore \s ->
  sfoldl foldOp (arithmetize y s) (payload y s) lHash
    (fmap (\ListItem {..} -> headLayout :*: headPayload)
      . unComp1 $ runPayloaded lWitness
    ) lSize
  where
    foldOp ::
      forall s.
      (SymbolicFold s, BaseField s ~ BaseField c) =>
      s (Layout y) -> Payload y (WitnessField s) ->
      s (Layout x :*: Payload x) ->
      (s (Layout y), Payload y (WitnessField s))
    foldOp yl yp il =
      let Switch {..} :: Switch s y = f @
                  ( Switch yl yp :: Switch s y
                  , Switch (hmap fstP il) (witnessF (hmap sndP il))
                      :: Switch s x)
       in (sLayout, sPayload)

revapp ::
  forall c x.
  (SymbolicOutput x, Context x ~ c, SymbolicFold c) =>
  List c x -> List c x -> List c x
-- | revapp xs ys = reverse xs ++ ys
revapp xs ys = foldl (Morph \(zs, x :: Switch s x) -> x .: zs) ys xs

reverse ::
  (SymbolicOutput x, Context x ~ c, SymbolicFold c) => List c x -> List c x
reverse xs = revapp xs emptyList

last :: (SymbolicOutput x, Context x ~ c, SymbolicFold c) => List c x -> x
last = head . reverse

init ::
  (SymbolicOutput x, Context x ~ c, SymbolicFold c) => List c x -> List c x
init = reverse . tail . reverse

(++) ::
  (SymbolicOutput x, Context x ~ c, SymbolicFold c) =>
  List c x -> List c x -> List c x
xs ++ ys = revapp (reverse xs) ys

foldr ::
  forall c x y.
  (SymbolicOutput x, Context x ~ c, SymbolicFold c) =>
  (SymbolicOutput y, Context y ~ c) =>
  MorphFrom c (x, y) y -> y -> List c x -> y
foldr f s xs =
  foldl (Morph \(y :: Switch s y, x :: Switch s x) ->
                    f @ (x, y) :: Switch s y) s (reverse xs)

filter ::
  forall c x.
  (SymbolicOutput x, Context x ~ c, SymbolicFold c) =>
  MorphFrom c x (Bool c) -> List c x -> List c x
filter pred = foldr (Morph \(x :: Switch s x, ys) ->
  ifThenElse (pred @ x :: Bool s) (x .: ys) ys) emptyList

delete ::
  forall c x.
  (SymbolicOutput x, Context x ~ c, SymbolicFold c) =>
  MorphFrom c (x, x) (Bool c) -> x -> List c x -> List c x
delete eq x xs =
  let (_, _, result) =
        foldr (Morph \(y :: Switch s x, (ok :: Bool s, y0 :: Switch s x, ys)) ->
          let test = eq @ (y, y0)
           in (ok || test, y0, ifThenElse (ok || not test) (y .: ys) ys))
           (false :: Bool c, x, emptyList) xs
   in result

setminus ::
  forall c x.
  (SymbolicOutput x, Context x ~ c, SymbolicFold c) =>
  MorphFrom c (x, x) (Bool c) -> List c x -> List c x -> List c x
setminus eq = foldl $ Morph \(xs, x :: Switch s x) ->
  delete (Morph \(y :: Switch s' x, z :: Switch s' x) ->
    eq @ (y, z) :: Bool s') x xs

singleton
    :: forall context x
    .  SymbolicOutput x
    => Context x ~ context
    => x
    -> List context x
singleton x = x .: emptyList

(!!) ::
  forall x c n.
  (SymbolicOutput x, Context x ~ c, SymbolicFold c) =>
  (KnownNat n, KnownRegisters c n Auto) =>
  List c x -> UInt n Auto c -> x
xs !! n = snd $ foldl (Morph \((m :: UInt n Auto s, y :: Switch s x), x) ->
  (m - one, ifThenElse (m == zero :: Bool s) x y))
  (n, restore $ const (embed $ pureRep zero, pureRep zero)) xs

concat ::
  forall c x.
  (SymbolicOutput x, Context x ~ c, SymbolicFold c) =>
  List c (List c x) -> List c x
concat xs = reverse $
  foldl (Morph \(ys, x :: List s (Switch s x)) -> revapp x ys) emptyList xs
