{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE NoDeriveAnyClass     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.UInt (
    StrictConv(..),
    StrictNum(..),
    UInt(..),
    OrdWord,
    toConstant,
    toNative,
    asWords,
    expMod,
    exp65537Mod,
    eea,
    natural,
    register,
    productMod,
    blueprintGE
) where

import           Control.Applicative               (Applicative (..))
import           Control.DeepSeq
import           Control.Monad                     (foldM, zipWithM)
import           Control.Monad.State               (StateT (..))
import           Data.Aeson                        (FromJSON (..), ToJSON (..))
import qualified Data.Bool                         as Haskell
import           Data.Foldable                     (Foldable (toList), foldlM, foldr, foldrM, for_)
import           Data.Function                     (on)
import           Data.Functor                      (Functor (..), (<$>))
import           Data.Functor.Rep                  (Representable (..))
import           Data.Kind                         (Type)
import           Data.List                         (unfoldr, zip)
import           Data.Map                          (fromList, (!))
import           Data.Maybe                        (fromJust)
import           Data.Traversable                  (for, traverse)
import           Data.Tuple                        (swap)
import qualified Data.Zip                          as Z
import           GHC.Generics                      (Generic, Par1 (..), (:*:) (..))
import           GHC.Natural                       (naturalFromInteger)
import           Prelude                           (Integer, const, error, flip, otherwise, return, type (~), ($), (++),
                                                    (.), (<>), (>>=))
import qualified Prelude                           as Haskell
import           Test.QuickCheck                   (Arbitrary (..), chooseInteger)

import           ZkFold.Algebra.Class              hiding (Euclidean (..))
import           ZkFold.Algebra.Field              (Zp)
import           ZkFold.Algebra.Number
import           ZkFold.Control.HApplicative       (HApplicative (..))
import           ZkFold.Data.HFunctor              (HFunctor (..))
import           ZkFold.Data.HFunctor.Classes      (HEq, HNFData, HShow)
import           ZkFold.Data.Product               (fstP, sndP)
import qualified ZkFold.Data.Vector                as V
import           ZkFold.Data.Vector                (Vector (..))
import           ZkFold.Prelude                    (length, replicate, replicateA, take, unsnoc)
import           ZkFold.Symbolic.Algorithm.FFT     (fft, ifft)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Class        (SymbolicData)
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import           ZkFold.Symbolic.Data.Input        (SymbolicInput, isValid)
import           ZkFold.Symbolic.Data.Ord
import           ZkFold.Symbolic.Interpreter       (Interpreter (..))
import           ZkFold.Symbolic.MonadCircuit

-- TODO (Issue #18): hide this constructor
newtype UInt (r :: Natural) (n :: Natural) (c :: (Type -> Type) -> Type) = UInt (c (Vector n))

deriving instance Generic (UInt r n c)
deriving instance HNFData c => NFData (UInt r n c)
deriving instance HEq c => Haskell.Eq (UInt r n c)
deriving instance HShow c => Haskell.Show (UInt r n c)
deriving newtype instance (KnownNat n, Symbolic c) => SymbolicData (UInt r n c)
deriving newtype instance (KnownNat n, Symbolic c) => Conditional (Bool c) (UInt r n c)
deriving newtype instance (KnownNat n, Symbolic c) => Eq (UInt r n c)

toNative ::
  forall r n c .
  (KnownNat r, Symbolic c) =>
  UInt r n c -> FieldElement c
toNative (UInt rs) = FieldElement $ symbolicF rs
  (Par1 . fromConstant . (`vectorToNatural` value @r))
  (fmap Par1 . hornerW @r . toList)

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => FromConstant Natural (UInt r n c) where
    fromConstant c = UInt . embed @c $ naturalToVector @r @n @c c

-- TODO: This one throws an underflow error if the value is negative. Should we do something about it?
instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => FromConstant Integer (UInt r n c) where
    fromConstant = fromConstant . naturalFromInteger

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => Scale Natural (UInt r n c)

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => Scale Integer (UInt r n c)

instance MultiplicativeMonoid (UInt r n c) => Exponent (UInt r n c) Natural where
    (^) = natPow

-- | @expMod n pow modulus@ calculates @n^pow % modulus@ where all values are arithmetised
--
expMod
    :: forall r n p m c .
    ( KnownNat r
    , KnownNat n
    , KnownNat p
    , KnownNat m
    , KnownNat (2 * m)
    , KnownNat (p * r)
    , Symbolic c
    , IsValidRegister r n c
    , IsValidRegister r p c
    , IsValidRegister r m c
    , IsValidRegister r (2 * m) c
    ) => UInt r n c -> UInt r p c -> UInt r m c -> UInt r m c
expMod n pow modulus = resize result
    where
        bits :: ByteString (p * r) c
        bits = from pow

        m' :: UInt r (2 * m) c
        m' = resize modulus

        n' :: UInt r (2 * m) c
        n' = resize n `mod` m'

        result :: UInt r (2 * m) c
        result = bitsPow (value @p) bits one n' m'

-- | n ^ 65537 `mod` modulus
--
exp65537Mod
    :: forall r n m c .
    ( KnownNat r
    , KnownNat n
    , KnownNat m
    , KnownNat (2 * m)
    , Symbolic c
    , IsValidRegister r n c
    , IsValidRegister r m c
    , IsValidRegister r (2 * m) c
    ) => UInt r n c -> UInt r m c -> UInt r m c
exp65537Mod n modulus = resize $ Haskell.snd $ productMod sq_2_16 n' m'
    where
        m' :: UInt r (2 * m) c
        m' = resize modulus

        n' :: UInt r (2 * m) c
        n' = resize n

        sq_2_16 = Haskell.foldl (\x _ -> Haskell.snd $ productMod x x m') n' [1..16 :: Natural]

bitsPow
    :: forall r n p c .
    ( KnownNat r
    , KnownNat n
    , KnownNat p
    , Symbolic c
    , IsValidRegister r n c
    ) => Natural -> ByteString p c -> UInt r n c -> UInt r n c -> UInt r n c -> UInt r n c
bitsPow 0 _ res _ _ = res
bitsPow b bits res n m = bitsPow (b -! 1) bits newRes sq m
    where
        sq = Haskell.snd $ productMod n n m
        newRes = force $ ifThenElse (isSet bits (b -! 1)) (Haskell.snd $ productMod res n m) res


-- | Calculate @a * b `divMod` m@ using less constraints than would've been required by these operations used consequently
--
productMod
    :: forall r n c .
    ( KnownNat r
    , KnownNat n
    , Symbolic c
    , IsValidRegister r n c
    ) => UInt r n c -> UInt r n c -> UInt r n c -> (UInt r n c, UInt r n c)
productMod (UInt aRegs) (UInt bRegs) (UInt mRegs) =
    case (value @n) of
      0 -> (zero, zero)
      _ -> (UInt $ hmap fstP circuit, UInt $ hmap sndP circuit)
      where
        source = symbolic3F aRegs bRegs mRegs
          (\ar br mr ->
            let r = value @r
                    -- value @r
                a' = vectorToNatural ar r
                b' = vectorToNatural br r
                m' = vectorToNatural mr r
            in naturalToVector @r @n @c ((a' * b') `div` m')
                :*: naturalToVector @r @n @c ((a' * b') `mod` m'))
          \ar br mr -> (liftA2 (:*:) `on` traverse unconstrained)
            (tabulate $ register @r @n @c ((natural @r @n @c ar * natural @r @n @c br) `div` natural @r @n @c mr))
            (tabulate $ register @r @n @c ((natural @r @n @c ar * natural @r @n @c br) `mod` natural @r @n @c mr))

        -- | Unconstrained @div@ part.
        dv = hmap fstP source

        -- | Unconstrained @mod@ part.
        md = hmap sndP source

        Bool eqCase = (UInt aRegs :: UInt r n c) `unsafeMulNoPad` UInt bRegs == UInt dv `unsafeMulNoPad` UInt mRegs + UInt md

        Bool ltCase = (UInt md :: UInt r n c) < UInt mRegs

        circuit = fromCircuit3F eqCase ltCase (dv `hpair` md) \(Par1 e) (Par1 l) dm -> do
          constraint (($ e) - one)
          constraint (($ l) - one)
          return dm


cast :: forall r n . (KnownNat r, KnownNat n) => Natural -> ([Natural], Natural, [Natural])
cast n =
    let base = 2 ^ value @r
        registers = flip unfoldr n $ \case
            0 -> Haskell.Nothing
            x -> Haskell.Just (swap $ x `Haskell.divMod` base)
        r = value @n
     in case greedySplitAt r registers of
        (lo, hi:rest) -> (lo, hi, rest)
        (lo, [])      -> (lo ++ replicate (r -! length lo) zero, zero, [])
    where
        greedySplitAt 0 xs = ([], xs)
        greedySplitAt _ [] = ([], [])
        greedySplitAt m (x : xs) =
            let (ys, zs) = greedySplitAt (m -! 1) xs
             in (x : ys, zs)

-- | Extended Euclidean algorithm.
-- Exploits the fact that @s_i@ and @t_i@ change signs in turns on each iteration, so it adjusts the formulas correspondingly
-- and never requires signed arithmetic.
-- (i.e. it calculates @x = b - a@ instead of @x = a - b@ when @a - b@ is negative
-- and changes @y - x@ to @y + x@ on the following iteration)
-- This only affects Bezout coefficients, remainders are calculated without changes as they are always non-negative.
--
-- If the algorithm is used to calculate Bezout coefficients,
-- it requires that @a@ and @b@ are coprime, @b@ is not 1 and @a@ is not 0, otherwise the optimisation above is not valid.
--
-- If the algorithm is only used to find @gcd(a, b)@ (i.e. @s@ and @t@ will be discarded), @a@ and @b@ can be arbitrary integers.
--
eea
    :: forall r n c .
    ( KnownNat r
    , KnownNat n
    , Symbolic c
    , IsValidRegister r n c
    ) => UInt r n c -> UInt r n c -> (UInt r n c, UInt r n c, UInt r n c)
eea a b = eea' 1 a b one zero zero one
    where
        iterations :: Natural
        iterations = value @n * 2 + 1

        eea' :: Natural -> UInt r n c -> UInt r n c -> UInt r n c -> UInt r n c -> UInt r n c -> UInt r n c -> (UInt r n c, UInt r n c, UInt r n c)
        eea' iteration oldR r oldS s oldT t
          | iteration Haskell.== iterations = (oldS, oldT, oldR)
          | otherwise = bool @(Bool c) rec (if Haskell.even iteration then b - oldS else oldS, if Haskell.odd iteration then a - oldT else oldT, oldR) (r == zero)
            where
                quotient = oldR `div` r

                rec = eea' (iteration + 1) r (oldR - quotient * r) s (quotient * s + oldS) t (quotient * t + oldT)


--------------------------------------------------------------------------------

instance (KnownNat r, Symbolic (Interpreter a)) => ToConstant (UInt r n (Interpreter a)) where
    type Const (UInt r n (Interpreter a)) = Natural
    toConstant (UInt (Interpreter xs)) = vectorToNatural xs (value @r)

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => MultiplicativeMonoid (UInt r n c) where
    one = fromConstant (1 :: Natural)

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => Semiring (UInt r n c)

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => Arbitrary (UInt r n c) where
    arbitrary
      | value @n == 0 = return zero
      | otherwise     = do
        lo <- replicateA (value @n -! 1) (toss $ value @r)
        hi <- toss (highRegisterSize @r @n)
        return $ UInt $ embed $ V.unsafeToVector (lo <> [hi])
        where toss b = fromConstant <$> chooseInteger (0, 2 ^ b - 1)

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c, m ~ n * r) => Iso (ByteString m c) (UInt r n c) where
    from (ByteString b)
      | value @n == 0 = zero
      | otherwise     = UInt $ symbolicF b
        (naturalToVector @r @n @c . Haskell.foldl (\y p -> toConstant p + 2 * y) 0)
        (\bits -> do
            let bsBits = V.fromVector bits
            V.unsafeToVector . Haskell.reverse <$> fromBits (highRegisterSize @r @n) (value @r) bsBits
        )

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c, m ~ n * r) => Iso (UInt r n c) (ByteString m c) where
    from (UInt u)
      | value @n == 0 = ByteString $ embed $ V.unsafeToVector []
      | otherwise     = ByteString $ symbolicF u
        (\v -> V.unsafeToVector $ fromConstant <$> toBsBits (vectorToNatural v (value @r)) (value @n))
        (\ui -> do
            let regs = V.fromVector ui
            V.unsafeToVector <$> toBits (Haskell.reverse regs) (highRegisterSize @r @n) (value @r)
        )

instance
    ( KnownNat r
    , KnownNat n
    , Log2 (Order (BaseField c) - 1) + 1 ~ n * r
    , IsValidRegister r n c
    , Symbolic c
    ) => Iso (FieldElement c) (UInt r n c) where
  from a = from (from a :: ByteString (n * r) c)

instance
    ( KnownNat r
    , KnownNat n
    , Log2 (Order (BaseField c) - 1) + 1 ~ n * r
    , IsValidRegister r n c
    , Symbolic c
    ) => Iso (UInt r n c) (FieldElement c) where
  from a = from (from a :: ByteString (n * r) c)

--------------------------------------------------------------------------------

instance
    ( KnownNat r
    , KnownNat n
    , KnownNat r'
    , KnownNat k
    , Symbolic c
    , IsValidRegister r n c
    , IsValidRegister r' k c
    ) => Resize (UInt r n c) (UInt r' k c) where
    resize (UInt bits)
      | value @n == 0 = zero
      | value @k == 0 = zero
      | otherwise     = UInt $ symbolicF bits
        (\l -> naturalToVector @r' @k @c (vectorToNatural l (value @r)))
        (\v -> do
            let regs = V.fromVector v
                ns = replicate (value @n -! 1) (value @r) ++ [highRegisterSize @r @n]
                ks = replicate (value @k -! 1) (value @r') ++ [highRegisterSize @r' @k]
                zs = zip ns regs

            resZ <- helper zs ks
            let (_, res) = Haskell.unzip resZ
            return $ V.unsafeToVector res
        )
        where
            helper :: MonadCircuit i (BaseField c) w m => [(Natural, i)] -> [Natural] -> m [(Natural, i)]
            helper _ [] = return []
            helper [] (a:as) = do
                ([(a, fromConstant @(BaseField c) zero)] <> ) Haskell.<$> helper [] as
            helper ((xN, xI) : xs) acc@(a:as)
                | xN > a = do
                        (l, h) <- splitExpansion a (xN -! a) xI
                        ([(a, l)] <> ) Haskell.<$> helper ((xN -! a, h) : xs) as
                | xN == a = ([(a, xI)] <> ) Haskell.<$> helper xs as
                | otherwise = case xs of
                    [] -> ([(xN, xI)] <> ) Haskell.<$> helper [] as
                    ((yN, yI) : ys) -> do
                        let newN = xN + yN
                        newI <- newAssigned (\j -> j xI + scale ((2 :: Natural) ^ xN) (j yI))
                        helper ((newN, newI) : ys) acc

-- | "natural" value from vector of registers.
natural ::
  forall r n c i.
  (KnownNat r, Symbolic c, Witness i (WitnessField c)) =>
  Vector n i -> IntegralOf (WitnessField c)
natural =
  foldr
    (\i c -> toIntegral (at i :: WitnessField c) + fromConstant base * c)
    zero
  where
    base :: Natural
    base = 2 ^ value @r

-- | @register n i@ returns @i@-th register of @n@.
register ::
  forall r n c . (KnownNat r, Symbolic c) =>
  IntegralOf (WitnessField c) ->
  Zp n -> WitnessField c
register c i =
  fromIntegral ((c `div` fromConstant (2 ^ shift :: Natural)) `mod` base)
  where
    rs = value @r
    base = fromConstant (2 ^ rs :: Natural)
    shift = toConstant i * rs

instance
    ( KnownNat r
    , KnownNat n
    , Symbolic c
    , IsValidRegister r n c
    ) => SemiEuclidean (UInt r n c) where
    divMod num@(UInt nm) den@(UInt dn) =
      (UInt $ hmap fstP circuit, UInt $ hmap sndP circuit)
      where

        -- | Computes unconstrained registers of @div@ and @mod@.
        source = symbolic2F nm dn
          (\n d ->
            let r = value @r
                n' = vectorToNatural n r
                d' = vectorToNatural d r
            in naturalToVector @r @n @c (n' `div` d')
                :*: naturalToVector @r @n @c (n' `mod` d'))
          \n d -> (liftA2 (:*:) `on` traverse unconstrained)
            (tabulate $ register @r @n @c (natural @r @n @c n `div` natural @r @n @c d))
            (tabulate $ register @r @n @c (natural @r @n @c n `mod` natural @r @n @c d))

        -- | Unconstrained @div@ part.
        dv = hmap fstP source

        -- | Unconstrained @mod@ part.
        md = hmap sndP source

        -- | divMod first constraint: @numerator = denominator * div + mod@.
        -- This should always be true.
        Bool eqCase = den * UInt dv + UInt md == num

        -- | divMod second constraint: @0 <= mod < denominator@.
        -- This should always be true.
        Bool ltCase = UInt md < den

        -- | Computes properly constrained registers of @div@ and @mod@.
        circuit = fromCircuit3F eqCase ltCase (dv `hpair` md) \(Par1 e) (Par1 l) dm -> do
          constraint (($ e) - one)
          constraint (($ l) - one)
          return dm

asWords
    :: forall regSize wordSize c k
    .  Symbolic c
    => KnownNat (Ceil regSize wordSize)
    => KnownNat wordSize
    => c (Vector k)                           -- @k@ registers of size up to @regSize@
    -> c (Vector (k * Ceil regSize wordSize)) -- @k * wordsPerReg@ registers of size @wordSize@
asWords v = fromCircuitF v $ \regs -> do
    words <- Haskell.mapM (expansionW @wordSize wordsPerReg) regs
    Haskell.pure $ V.reverse . V.unsafeToVector . Haskell.concat . V.fromVector $ words
  where
      wordsPerReg :: Natural
      wordsPerReg = value @(Ceil regSize wordSize)

-- | Word size in bits used in comparisons. Subject to change
type OrdWord = 16

instance
    ( KnownNat r
    , KnownNat n
    , Symbolic c
    ) => Ord (UInt r n c) where

    type OrderingOf (UInt r n c) = Ordering c

    ordering x y z o = bool (bool x y (o == eq)) z (o == gt)

    compare x y = bool (bool lt eq (x == y)) gt (x > y)

    x <= y = y >= x

    x <  y = y > x

    (UInt u1) >= (UInt u2) =
        let w1 = withCeilRegSize @r @OrdWord $ asWords @r @OrdWord u1
            w2 = withCeilRegSize @r @OrdWord $ asWords @r @OrdWord u2
         in bitwiseGE @OrdWord w1 w2

    (UInt u1) > (UInt u2) =
        let w1 = withCeilRegSize @r @OrdWord $ asWords @r @OrdWord u1
            w2 = withCeilRegSize @r @OrdWord $ asWords @r @OrdWord u2
         in bitwiseGT @OrdWord w1 w2

    max x y = bool @(Bool c) x y $ x < y

    min x y = bool @(Bool c) x y $ x > y

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => AdditiveSemigroup (UInt r n c) where
    UInt xc + UInt yc
      | value @n == 0 = zero
      | otherwise     = UInt $ symbolic2F xc yc
        (\u v -> naturalToVector @r @n @c $ vectorToNatural u (value @r) + vectorToNatural v (value @r))
        (\xv yv -> do
            j <- newAssigned (Haskell.const zero)
            let xs = V.fromVector xv
                ys = V.fromVector yv
                midx = Haskell.init xs
                z    = Haskell.last xs
                midy = Haskell.init ys
                w    = Haskell.last ys
            (zs, c) <- flip runStateT j $ traverse StateT $
                Haskell.zipWith (fullAdder $ value @r) midx midy
            k <- fullAdded z w c
            (ks, _) <- splitExpansion (highRegisterSize @r @n) 1 k
            return $ V.unsafeToVector (zs ++ [ks])
        )

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => AdditiveMonoid (UInt r n c) where
    zero = fromConstant (0 :: Natural)

instance
    ( KnownNat r
    , KnownNat n
    , Symbolic c
    , IsValidRegister r n c
    ) => AdditiveGroup (UInt r n c) where
    UInt x - UInt y = UInt $ symbolic2F x y
        (\u v -> naturalToVector @r @n @c $ vectorToNatural u (value @r) + 2 ^ (value @n) -! vectorToNatural v (value @r))
        (\xv yv -> do
            let is = V.fromVector xv
                js = V.fromVector yv
            case Z.zip is js of
                []              -> return $ V.unsafeToVector []
                [(i, j)]        -> V.unsafeToVector <$> solve1 i j
                ((i, j) : rest) -> let (z, w) = Haskell.last rest
                                       (ris, rjs) = Haskell.unzip $ Haskell.init rest
                                    in V.unsafeToVector <$> solveN (i, j) (ris, rjs) (z, w)
        )
        where
            t :: BaseField c
            t = (one + one) ^ value @r

            solve1 :: MonadCircuit i (BaseField c) w m => i -> i -> m [i]
            solve1 i j = do
                z0 <- newAssigned (\v -> v i - v j + fromConstant t)
                (z, _) <- splitExpansion (highRegisterSize @r @n) 1 z0
                return [z]

            solveN :: MonadCircuit i (BaseField c) w m => (i, i) -> ([i], [i]) -> (i, i) -> m [i]
            solveN (i, j) (is, js) (i', j') = do
                s <- newAssigned (\v -> v i - v j + fromConstant t)
                let r = value @r
                (k, b0) <- splitExpansion r 1 s
                (zs, b) <- flip runStateT b0 $ traverse StateT (Haskell.zipWith (fullSub r) is js)
                d <- newAssigned (\v -> v i' - v j')
                s'0 <- newAssigned (\v -> v d + v b + fromConstant (2 ^ highRegisterSize @r @n -! 1 :: Natural))
                (s', _) <- splitExpansion (highRegisterSize @r @n) 1 s'0
                return (k : zs <> [s'])

    negate :: UInt r n c -> UInt r n c
    negate (UInt x) = UInt $ symbolicF x
        (\v -> naturalToVector @r @n @c $ (2 ^ value @n) -! vectorToNatural v (value @r))
        (\xv -> if Haskell.null (V.fromVector xv)
            then return xv
            else do
                j <- newAssigned (Haskell.const zero)
                let xs = V.fromVector xv
                    y = 2 ^ value @r
                    ys = replicate (value @n -! 2) (2 ^ value @r -! 1)
                    y' = 2 ^ highRegisterSize @r @n -! 1
                    ns
                        | value @n Haskell.== 1 = [y' + 1]
                        | otherwise = (y : ys) <> [y']
                    (init_ns, last_ns) = fromJust $ unsnoc ns
                    (init_xs, last_xs) = fromJust $ unsnoc xs
                (zs, p) <- flip runStateT j $ traverse StateT (Haskell.zipWith negateN init_ns init_xs)
                (zp_head, _) <- negateNH last_ns last_xs p
                return $ V.unsafeToVector (zs <> [zp_head])
        )
        where
            negateN :: MonadCircuit i (BaseField c) w m => Natural -> i -> i -> m (i, i)
            negateN n i b = do
                r <- newAssigned (\v -> fromConstant n - v i + v b)
                splitExpansion (value @r) 1 r

            negateNH :: MonadCircuit i (BaseField c) w m => Natural -> i -> i -> m (i, i)
            negateNH n i b = do
                r <- newAssigned (\v -> fromConstant n - v i + v b)
                splitExpansion (highRegisterSize @r @n) 1 r

instance
    (KnownNat r
    , KnownNat n
    , Symbolic c
    ) => MultiplicativeSemigroup (UInt r n c) where
    UInt x * UInt y = UInt $
        case (value @n) of
          0 -> x
          _ -> withSecondNextNBits @n $ trimRegisters @r @n $ mulFFT @c xPadded yPadded
        where
            xPadded, yPadded :: c (Vector (SecondNextPow2 n))
            xPadded = fromCircuitF x padSecondNextPow2
            yPadded = fromCircuitF y padSecondNextPow2

-- | Multiply two UInts assuming neither of them holds a value of more than @n / 2@ bits.
-- Requires less constraints than regular multiplication but its behaviour is undefined if the assumption does not hold.
-- Intended for internal usage
--
unsafeMulNoPad
    :: forall r n c .
    ( KnownNat r
    , KnownNat n
    , Symbolic c
    ) => UInt r n c -> UInt r n c -> UInt r n c
unsafeMulNoPad (UInt x) (UInt y) = UInt $
    case (value @n) of
      0 -> x
      _ -> withNextNBits @n $ trimRegisters @r @n $ mulFFT @c xPadded yPadded
    where
        xPadded, yPadded :: c (Vector (NextPow2 n))
        xPadded = fromCircuitF x padNextPow2
        yPadded = fromCircuitF y padNextPow2


trimRegisters
    :: forall (r :: Natural) n k c
    .  Symbolic c
    => KnownNat r
    => KnownNat n
    => c (Vector (2^k)) -> c (Vector n)
trimRegisters c = fromCircuitF c $ \regs -> do
    let rs   = take (value @n) $ V.fromVector regs
        lows = Haskell.init rs
        hi   = Haskell.last rs
    z <- newAssigned (const zero)
    (newLows, carry) <- foldlM step ([], z) lows

    let highOverflow = value @r + maxOverflow @r @n -! highRegisterSize @r @n
    s <- newAssigned (\p -> p carry + p hi)
    (newHi, _) <- splitExpansion (highRegisterSize @r @n) highOverflow s

    pure $ V.unsafeToVector $ Haskell.reverse (newHi : newLows)

    where
        step :: forall i w m. MonadCircuit i (BaseField c) w m => ([i], i) -> i -> m ([i], i)
        step (acc, cr) r = do
            s <- newAssigned (\p -> p cr + p r)
            (l, h) <- splitExpansion (value @r) (maxOverflow @r @n) s
            pure (l : acc, h)

mulFFT
    :: forall c k
    .  Symbolic c
    => KnownNat k
    => c (Vector (2^k)) -> c (Vector (2^k)) -> c (Vector (2^k))
mulFFT x y = c
    where
        xHat, yHat :: c (Vector (2^k))
        xHat = fft x
        yHat = fft y

        c :: c (Vector (2^k))
        c = ifft $ fromCircuit2F xHat yHat $ \u v ->
                V.unsafeToVector <$> zipWithM (\i j -> newAssigned $ \p -> p i * p j) (V.fromVector u) (V.fromVector v)


instance
    ( KnownNat r
    , KnownNat n
    , Symbolic c
    , IsValidRegister r n c
    ) => Ring (UInt r n c)

--------------------------------------------------------------------------------

class StrictConv b a where
    strictConv :: b -> a

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => StrictConv Natural (UInt r n c) where
    strictConv n
      | value @n == 0 = zero
      | otherwise     = case cast @r @n n of
        (lo, hi, []) -> UInt $ embed $ V.unsafeToVector $ fromConstant <$> (lo <> [hi])
        _            -> error "strictConv: overflow"

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => StrictConv (Zp p) (UInt r n c) where
    strictConv = strictConv . toConstant

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => StrictConv (c Par1) (UInt r n c) where
    strictConv a = UInt $ symbolicF a (\p -> V.unsafeToVector [unPar1 p]) (\xv -> do
        let i = unPar1 xv
            len = Haskell.min (getNatural @n) (numberOfBits @(BaseField c))
        bits <- Haskell.reverse <$> expansion len i
        V.unsafeToVector <$> fromBits (highRegisterSize @r @n) (value @r) bits)


class StrictNum a where
    strictAdd :: a -> a -> a
    strictSub :: a -> a -> a
    strictMul :: a -> a -> a

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => StrictNum (UInt r n c) where
    strictAdd (UInt x) (UInt y)
      | value @n == 0 = zero
      | otherwise     = UInt $ symbolic2F x y
        (\u v -> naturalToVector @r @n @c $ vectorToNatural u (value @r) + vectorToNatural v (value @r))
        (\xv yv -> do
            j <- newAssigned (Haskell.const zero)
            let xs = V.fromVector xv
                ys = V.fromVector yv
                z    = Haskell.last xs
                w    = Haskell.last ys
                midx = Haskell.init xs
                midy = Haskell.init ys
            (zs, c) <- flip runStateT j $ traverse StateT $
                Haskell.zipWith (fullAdder $ value @r) midx midy
            k <- fullAdded z w c
            (ks, _) <- splitExpansion (highRegisterSize @r @n) 1 k
            return $ V.unsafeToVector (zs ++ [ks])
        )

    strictSub (UInt x) (UInt y) = UInt $ symbolic2F x y
        (\u v -> naturalToVector @r @n @c $ vectorToNatural u (value @r) -! vectorToNatural v (value @r))
        (\xv yv -> do
            case V.fromVector $ Z.zip xv yv of
              []              -> return $ V.unsafeToVector []
              [(i, j)]        -> V.unsafeToVector <$> solve1 i j
              ((i, j) : rest) -> let (z, w) = Haskell.last rest
                                     (ris, rjs) = Haskell.unzip $ Haskell.init rest
                                  in V.unsafeToVector <$> solveN (i, j) (ris, rjs) (z, w)
        )
        where
            t :: BaseField c
            t = (one + one) ^ value @r - one

            solve1 :: MonadCircuit i (BaseField c) w m => i -> i -> m [i]
            solve1 i j = do
                z <- newAssigned (\v -> v i - v j)
                _ <- expansion (highRegisterSize @r @n) z
                return [z]

            solveN :: MonadCircuit i (BaseField c) w m => (i, i) -> ([i], [i]) -> (i, i) -> m [i]
            solveN (i, j) (is, js) (i', j') = do
                s <- newAssigned (\v -> v i - v j + fromConstant (t + one))
                let r = value @r
                (k, b0) <- splitExpansion r 1 s
                (zs, b) <- flip runStateT b0 $ traverse StateT (Haskell.zipWith (fullSub r) is js)
                k' <- newAssigned (\v -> v i' - v j')
                s' <- newAssigned (\v -> v k' + v b - one)
                _ <- expansion (highRegisterSize @r @n) s'
                return (k : zs <> [s'])

    strictMul (UInt x) (UInt y) = UInt $ symbolic2F x y
        (\u v -> naturalToVector @r @n @c $ vectorToNatural u (value @r) * vectorToNatural v (value @r))
        (\xv yv -> do
                case V.fromVector $ Z.zip xv yv of
                  []              -> return $ V.unsafeToVector []
                  [(i, j)]        -> V.unsafeToVector <$> solve1 i j
                  ((i, j) : rest) -> let (z, w) = Haskell.last rest
                                         (ris, rjs) = Haskell.unzip $ Haskell.init rest
                                      in V.unsafeToVector <$> solveN (i, j) (ris, rjs) (z, w)
        )
        where
            solve1 :: MonadCircuit i (BaseField c) w m => i -> i -> m [i]
            solve1 i j = do
                z <- newAssigned $ \v -> v i * v j
                _ <- expansion (highRegisterSize @r @n) z
                return [z]

            solveN :: MonadCircuit i (BaseField c) w m => (i, i) -> ([i], [i]) -> (i, i) -> m [i]
            solveN (i, j) (is, js) (i', j') = do
                let cs = fromList $ zip [0..] (i : is ++ [i'])
                    ds = fromList $ zip [0..] (j : js ++ [j'])
                    n = value @n
                -- single addend for lower register
                q <- newAssigned (\v -> v i * v j)
                -- multiple addends for middle registers
                qs <- for [1 .. n -! 2] $ \k ->
                    for [0 .. k] $ \l ->
                        newAssigned (\v -> v (cs ! l) * v (ds ! (k -! l)))
                -- lower register
                (p, c) <- splitExpansion (value @r) (value @r) q
                -- middle registers
                (ps, c') <- flip runStateT c $ for qs $ StateT . \rs c' -> do
                    s <- foldrM (\k l -> newAssigned (\v -> v k + v l)) c' rs
                    splitExpansion (value @r) (maxOverflow @r @n) s
                -- high register
                p' <- foldrM (\k l -> do
                    k' <- newAssigned (\v -> v (cs ! k) * v (ds ! (n -! (k + 1))))
                    newAssigned (\v -> v l + v k')) c' [0 .. n -! 1]
                _ <- expansion (highRegisterSize @r @n) p'
                -- all addends higher should be zero
                for_ [n .. n * 2 -! 2] $ \k ->
                    for_ [k -! n + 1 .. n -! 1] $ \l ->
                        constraint (\v -> v (cs ! l) * v (ds ! (k -! l)))
                return (p : ps <> [p'])

instance
  ( KnownNat r
  , KnownNat n
  , Symbolic c
  ) => SymbolicInput (UInt r n c) where
    isValid (UInt bits)
      | value @n == 0 = true
      | otherwise     = Bool $ fromCircuitF bits $ \v -> do
        let vs = V.fromVector v
        bs <- toBits (Haskell.reverse vs) (highRegisterSize @r @n) (value @r)
        ys <- Haskell.reverse <$> fromBits (highRegisterSize @r @n) (value @r) bs
        difference <- for (zip vs ys) $ \(i, j) -> newAssigned (\w -> w i - w j)
        isZeros <- for difference $ isZero . Par1
        case isZeros of
            []       -> Par1 <$> newAssigned (const one)
            (z : zs) -> foldlM (\(Par1 v1) (Par1 v2) -> Par1 <$> newAssigned (($ v1) * ($ v2))) z zs

--------------------------------------------------------------------------------

fullAdder :: (Arithmetic a, MonadCircuit i a w m) => Natural -> i -> i -> i -> m (i, i)
fullAdder r xk yk c = fullAdded xk yk c >>= splitExpansion r 1

fullAdded :: MonadCircuit i a w m => i -> i -> i -> m i
fullAdded i j c = do
    k <- newAssigned (\v -> v i + v j)
    newAssigned (\v -> v k + v c)

fullSub :: (Arithmetic a, MonadCircuit i a w m) => Natural -> i -> i -> i -> m (i, i)
fullSub r xk yk b = do
    d <- newAssigned (\v -> v xk - v yk)
    s <- newAssigned (\v -> v d + v b + (one + one) ^ r - one)
    splitExpansion r 1 s

naturalToVector :: forall r n c . (KnownNat r, KnownNat n, Symbolic c) => Natural -> Vector n (BaseField c)
naturalToVector c
  | value @n == 0 = V.unsafeToVector []
  | otherwise     = let nBits = value @n * value @r
                        (lo, hi, _) = cast @r @n . (`Haskell.mod` (2 ^ nBits)) $ c
                     in V.unsafeToVector $ (fromConstant <$> lo) <> [fromConstant hi]

vectorToNatural :: (ToConstant a, Const a ~ Natural) => Vector n a -> Natural -> Natural
vectorToNatural v n = foldr (\l r -> fromConstant l  + b * r) 0 vs where
    vs = Haskell.map toConstant $ V.fromVector v :: [Natural]
    b = 2 ^ n

instance (KnownNat r, KnownNat n, Symbolic c, IsValidRegister r n c) => FromJSON (UInt r n c) where
    parseJSON = Haskell.fmap strictConv . parseJSON @Natural

instance (KnownNat r, KnownNat n, Symbolic (Interpreter (Zp p))) => ToJSON (UInt r n (Interpreter (Zp p))) where
    toJSON = toJSON . toConstant


-- -- Old Ord circuits for compatibility --

bitwiseGE :: forall r c f . (Symbolic c, Z.Zip f, Foldable f, KnownNat r) => c f -> c f -> Bool c
-- ^ Given two lists of bits of equal length, compares them lexicographically.
bitwiseGE xs ys = Bool $
  symbolic2F xs ys
    (\us vs -> Par1 $ Haskell.bool zero one (toList us Haskell.>= toList vs))
    $ \is js -> Par1 <$> blueprintGE @r is js

blueprintGE :: forall r i a w m f . (Arithmetic a, MonadCircuit i a w m, Z.Zip f, Foldable f, KnownNat r) => f i -> f i -> m i
blueprintGE xs ys = do
  (_, hasNegOne) <- circuitDelta @r xs ys
  newAssigned $ \p -> one - p hasNegOne

bitwiseGT :: forall r c f . (Symbolic c, Z.Zip f, Foldable f, KnownNat r) => c f -> c f -> Bool c
-- ^ Given two lists of bits of equal length, compares them lexicographically.
bitwiseGT xs ys = Bool $
  symbolic2F xs ys
    (\us vs -> Par1 $ Haskell.bool zero one (toList us Haskell.> toList vs))
    $ \is js -> do
      (hasOne, hasNegOne) <- circuitDelta @r is js
      Par1 <$> newAssigned (\p -> p hasOne * (one - p hasNegOne))

-- | Compare two sets of r-bit words lexicographically
--
circuitDelta :: forall r i a w m f . (Arithmetic a, MonadCircuit i a w m, Z.Zip f, Foldable f, KnownNat r) => f i -> f i -> m (i, i)
circuitDelta l r = do
    z1 <- newAssigned (Haskell.const zero)
    z2 <- newAssigned (Haskell.const zero)
    foldM update (z1, z2) $ Z.zip l r
        where
            bound = scale ((2 ^ value @r) -! 1) one

            -- | If @z1@ is set, there was an index i where @xs[i] == 1@ and @ys[i] == 0@ and @xs[j] == ys[j]@ for all j < i.
            -- In this case, no matter what bit states are after this index, @z1@ and @z2@ are not updated.
            --
            --   If @z2@ is set, there was an index i where @xs[i] == 0@ and @ys[i] == 1@ and @xs[j] == ys[j]@ for all j < i.
            -- In the same manner, @z1@ and @z2@ won't be updated afterwards.
            update :: (i, i) -> (i, i) -> m (i, i)
            update (z1, z2) (x, y) = do
                -- @f1@ is one if and only if @x > y@ and zero otherwise.
                -- @(y + 1) `div` (x + 1)@ is zero if and only if @y < x@ regardless of whether @x@ is zero.
                -- @x@ and @y@ are expected to be of at most @r@ bits where @r << NumberOfBits a@, so @x + 1@ will not be zero either.
                -- Because of our laws for @finv@, @q // q@ is 1 if @q@ is not zero, and zero otherwise.
                -- This is exactly the opposite of what @f1@ should be.
                f1 <- newRanged one $
                    let q = fromIntegral (toIntegral (at y + one @w) `div` toIntegral (at x + one @w))
                     in one - q // q

                -- f2 is one if and only if y > x and zero otherwise
                f2 <- newRanged one $
                    let q = fromIntegral (toIntegral (at x + one @w) `div` toIntegral (at y + one @w))
                     in one - q // q

                dxy <- newAssigned (\p -> p x - p y)

                d1  <- newAssigned (\p -> p f1 * p dxy - p f1)
                d1' <- newAssigned (\p -> (one - p f1) * negate (p dxy))
                rangeConstraint d1  bound
                rangeConstraint d1' bound

                d2  <- newAssigned (\p -> p f2 * (negate one - p dxy))
                d2' <- newAssigned (\p -> p dxy - p f2 * p dxy)
                rangeConstraint d2  bound
                rangeConstraint d2' bound

                bothZero <- newAssigned $ \p -> (one - p z1) * (one - p z2)

                f1z <- newAssigned $ \p -> p bothZero * p f1
                f2z <- newAssigned $ \p -> p bothZero * p f2

                z1' <- newAssigned $ \p -> p z1 + p f1z
                z2' <- newAssigned $ \p -> p z2 + p f2z

                Haskell.return (z1', z2')
