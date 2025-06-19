{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators    #-}

module ZkFold.Symbolic.Data.List where

import           Control.Monad                     (return)
import           Data.Distributive                 (Distributive (..))
import           Data.Function                     (const, ($), (.))
import           Data.Functor                      (Functor (..), (<$>))
import           Data.Functor.Rep                  (Representable (..), pureRep, tabulate)
import           Data.List                         (zipWith)
import           Data.List.Infinite                (Infinite (..))
import           Data.Proxy                        (Proxy (..))
import           Data.Traversable                  (traverse)
import           Data.Tuple                        (fst, snd)
import           Data.Type.Equality                (type (~))
import           GHC.Generics                      (Generic, Generic1, Par1 (..), (:*:) (..), (:.:) (..))
import           Prelude                           (undefined)
import qualified Prelude                           as Haskell

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Number             (KnownNat)
import           ZkFold.Control.HApplicative       (hliftA3)
import           ZkFold.Data.HFunctor              (hmap)
import           ZkFold.Data.List.Infinite         ()
import           ZkFold.Data.Orphans               ()
import           ZkFold.Data.Package               (pack, unpack)
import           ZkFold.Data.Product               (fstP, sndP)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool         (Bool (..), BoolType (..))
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Conditional  (Conditional (..), ifThenElse)
import           ZkFold.Symbolic.Data.Eq           (Eq (..))
import           ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import           ZkFold.Symbolic.Data.Input        (SymbolicInput (..))
import           ZkFold.Symbolic.Data.Morph        (MorphFrom, MorphTo (..), (@))
import           ZkFold.Symbolic.Data.Switch       (Switch (..))
import           ZkFold.Symbolic.Data.UInt         (UInt)
import           ZkFold.Symbolic.Data.Witness      (Wit (..))
import           ZkFold.Symbolic.Fold
import           ZkFold.Symbolic.MonadCircuit

data ListItem f a = ListItem
  { tailHash   :: f a
  , headLayout :: f a
  }
  deriving (Functor, Generic1, Representable)

instance (Distributive f) => Distributive (ListItem f) where
  distribute f = ListItem
    { tailHash = distribute (tailHash <$> f)
    , headLayout = distribute (headLayout <$> f)
    }

data List x c = List
  { lHash    :: x c
  , lSize    :: FieldElement c
  , lWitness :: [Wit x c]
  }
  deriving (Generic, Generic1)

instance SymbolicDataConstraint x => SymbolicData (List x) where
  type Layout (List x) a = Layout x a :*: Layout FieldElement a :*: ([] :.: Layout x a)
  toContext List{..} =
    let w = pack $ fmap toContext lWitness
        h = toContext lHash
        s = toContext lSize
    in hliftA3 (\a b c -> a :*: b :*: c) h s w
  fromContext c =
    let lHash :*: lSize :*: Comp1 lWitness = fromContext c
    in List { lHash, lSize, lWitness }
-- | TODO: Maybe some 'isValid' check for Lists?..
instance SymbolicDataConstraint x => SymbolicInput (List x) where
  isValid _ = true

instance (SymbolicDataConstraint x, Symbolic c) => Conditional (Bool c) (List x c)

-- | TODO: A proof-of-concept where hash == id.
-- Replace id with a proper hash if we need lists to be cryptographically secure.
--
emptyList
    :: forall x c
    .  SymbolicDataConstraint x
    => Symbolic c
    => List x c
emptyList =
  List (fromContext $ embed $ pureRep zero) zero []

null
    :: forall x c
    .  Symbolic c
    => List x c
    -> Bool c
null List{..} = lSize == zero

infixr 5 .:
(.:)
    :: forall x c
    .  SymbolicDataConstraint x
    => Symbolic c
    => x c
    -> List x c
    -> List x c
x .: List{..} = List incHash incSize incWitness
  where
    incHash    = fromContext $ fromCircuit3F (toContext lHash) (toContext x) (toContext incSize) \vHash vRepr (Par1 s) ->
      mzipWithMRep (hashFun s) vHash vRepr
    incSize    = lSize + one
    incWitness = fromContext (toContext x) : lWitness

hashFun :: MonadCircuit i a w m => i -> i -> i -> m i
hashFun s h t = newAssigned (($ h) + ($ t) * ($ s))

-- | TODO: Is there really a nicer way to handle empty lists?
--
uncons ::
  forall x c .
  SymbolicDataConstraint x =>
  Symbolic c =>
  List x c -> (x c, List x c)
uncons (List incHash incSize incWitness) = (x, List {..})
  where
    x :: x c
    lWitness :: [Wit x c]
    (x, lWitness) = case incWitness of
      []                 -> (fromContext $ embed $ pureRep zero, [])
      (Wit w : tWitness) -> (fromContext $ embedW w, tWitness)
    lHash  = fromContext $ fromCircuit3F (toContext incHash) (toContext x) (toContext incSize) \vHash vRepr (Par1 s) ->
      mzipWithMRep (hashFun s) vHash vRepr
    lSize  = incSize - one

head ::
  SymbolicDataConstraint x =>
  Symbolic c =>
  List x c -> x c
head = fst . uncons

tail ::
  SymbolicDataConstraint x =>
  Symbolic c =>
  List x c -> List x c
tail = snd . uncons

-- TODO: Modify `SymbolicFold` to store witness data in-circuit.
foldl ::
  forall x y c.
  ( SymbolicData x
  , SymbolicData y
  , SymbolicFold c
  ) =>
  MorphFrom c (y :*: x) y -> y c -> List x c -> y c
foldl _ _ _= undefined
-- foldl f y List {..} = restore \s ->
--   sfoldl foldOp (arithmetize y s) (payload y s) lHash
--     (fmap (\ListItem {..} -> headLayout :*: headPayload)
--       . unComp1 $ runPayloaded lWitness
--     ) lSize
--   where
--     foldOp ::
--       forall s.
--       (SymbolicFold s, BaseField s ~ BaseField c) =>
--       s (Layout y) -> Payload y (WitnessField s) ->
--       s (Layout x :*: Payload x) ->
--       (s (Layout y), Payload y (WitnessField s))
--     foldOp yl yp il =
--       let Switch {..} :: Switch s y = f @
--                   ( Switch yl yp :: Switch s y
--                   , Switch (hmap fstP il) (witnessF (hmap sndP il))
--                       :: Switch s x)
--        in (sLayout, sPayload)

revapp ::
  forall c x.
  (SymbolicDataConstraint x, SymbolicFold c) =>
  List x c -> List x c -> List x c
-- | revapp xs ys = reverse xs ++ ys
revapp xs ys = foldl (Morph \(zs :*: x) -> x .: zs) ys xs

reverse ::
  (SymbolicDataConstraint x, SymbolicFold c) => List x c -> List x c
reverse xs = revapp xs emptyList

last :: (SymbolicDataConstraint x, SymbolicFold c) => List x c -> x c
last = head . reverse

init ::
  (SymbolicDataConstraint x, SymbolicFold c) => List x c -> List x c
init = reverse . tail . reverse

(++) ::
  (SymbolicDataConstraint x, SymbolicFold c) =>
  List x c -> List x c -> List x c
xs ++ ys = revapp (reverse xs) ys

foldr ::
  forall c x y.
  (SymbolicDataConstraint x, SymbolicFold c) =>
  (SymbolicData y) =>
  MorphFrom c (x :*: y) y -> y c -> List x c -> y c
foldr f s xs =
  foldl (Morph \(y :*: x) ->
                    f @ (x :*: y)) s (reverse xs)

filter ::
  forall c x.
  (SymbolicDataConstraint x, SymbolicFold c) =>
  MorphFrom c x Bool -> List x c -> List x c
filter pred = foldr (Morph \(x :*: ys) ->
  ifThenElse (pred @ x) (x .: ys) ys) emptyList

delete ::
  forall c x.
  (SymbolicDataConstraint x, SymbolicFold c) =>
  MorphFrom c (x :*: x) Bool -> x c -> List x c -> List x c
delete eq x xs =
  let _ :*: _ :*: result =
        foldr (Morph \(y :*: (ok :*: y0 :*: ys)) ->
          let test = eq @ (y :*: y0)
           in ((ok || test) :*: y0 :*: (ifThenElse (ok || not test) (y .: ys) ys)))
           (false @(Bool c) :*: x :*: emptyList) xs
   in result

setminus ::
  forall c x.
  (SymbolicDataConstraint x, SymbolicFold c) =>
  MorphFrom c (x :*: x) Bool -> List x c -> List x c -> List x c
setminus eq = foldl $ Morph \(xs :*: x) ->
  delete (Morph \(y :*: z) -> eq @ (y :*: z)) x xs

singleton
    :: forall x c
    .  SymbolicDataConstraint x
    => Symbolic c
    => x c
    -> List x c
singleton x = x .: emptyList

(!!) ::
  forall x c .
  (SymbolicDataConstraint x, SymbolicFold c) =>
  List x c -> FieldElement c -> x c
xs !! n = sndP $ foldl (Morph \((m :*: y) :*: x) ->
  ((m - one) :*: (ifThenElse (m == zero) x y)))
  (n :*: fromContext (embed $ pureRep zero)) xs

concat ::
  forall x c .
  (SymbolicDataConstraint x, SymbolicFold c) =>
  List (List x) c -> List x c
concat xs = reverse $
  foldl (Morph \(ys :*: x) -> revapp x ys) emptyList xs

-- TODO: Seems like we should return a `Maybe` or `p-1` if the index is not found.
findIndex ::
  forall x c .
  (SymbolicData x, SymbolicFold c) =>
  MorphFrom c x (Bool) -> List x c -> FieldElement c
findIndex p xs = sndP $ foldl (Morph \((m :*: y) :*: x) ->
  ((m + one) :*: (ifThenElse (p @ x) m y)))
  ((zero :: FieldElement c) :*: zero) xs

insert ::
  forall x c .
  (SymbolicDataConstraint x, SymbolicFold c) =>
  List x c -> FieldElement c -> x c -> List x c
insert xs n xi =
  let _ :*:  _ :*: res = foldr (Morph \(a :*: (n' :*: xi' :*: l')) ->
        ((n' - one) :*: xi' :*: ifThenElse (n' == zero) (xi' .: l') (a .: l'))) (n :*: xi :*: emptyList) xs
   in res

