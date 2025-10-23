{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Data.Summoner where

import Data.Constraint (Dict (..), (\\))
import Data.Constraint.Nat
import GHC.TypeLits

data NumExpr
  = NConst Natural
  | NumExpr :+ NumExpr
  | NumExpr :-! NumExpr
  | NumExpr :* NumExpr
  | NumExpr :^ NumExpr
  | NMin NumExpr NumExpr
  | NMax NumExpr NumExpr
  | NGcd NumExpr NumExpr
  | NLcm NumExpr NumExpr
  | NumExpr :/ NumExpr
  | NumExpr :% NumExpr
  | NLog2 NumExpr

type family Eval (e :: NumExpr) :: Natural where
  Eval (NConst n) = n
  Eval (x :+ y) = Eval x + Eval y
  Eval (x :-! y) = Eval x - Eval y
  Eval (x :* y) = Eval x * Eval y
  Eval (x :^ y) = Eval x ^ Eval y
  Eval (NMin x y) = Eval x `Min` Eval y
  Eval (NMax x y) = Eval x `Max` Eval y
  Eval (NGcd x y) = Eval x `Gcd` Eval y
  Eval (NLcm x y) = Eval x `Lcm` Eval y
  Eval (x :/ y) = Eval x `Div` Eval y
  Eval (x :% y) = Eval x `Mod` Eval y
  Eval (NLog2 x) = Log2 (Eval x)

class Summon (x :: NumExpr) where
  summon :: Dict (KnownNat (Eval x))

instance KnownNat n => Summon (NConst n) where
  summon = Dict

instance (Summon x, Summon y) => Summon (x :+ y) where
  summon = Dict \\ plusNat @(Eval x) @(Eval y) \\ summon @x \\ summon @y

instance (Summon x, Summon y, Eval y <= Eval x) => Summon (x :-! y) where
  summon = Dict \\ minusNat @(Eval x) @(Eval y) \\ summon @x \\ summon @y

instance (Summon x, Summon y) => Summon (x :* y) where
  summon = Dict \\ timesNat @(Eval x) @(Eval y) \\ summon @x \\ summon @y

instance (Summon x, Summon y) => Summon (x :^ y) where
  summon = Dict \\ powNat @(Eval x) @(Eval y) \\ summon @x \\ summon @y

instance (Summon x, Summon y) => Summon (NMin x y) where
  summon = Dict \\ minNat @(Eval x) @(Eval y) \\ summon @x \\ summon @y

instance (Summon x, Summon y) => Summon (NMax x y) where
  summon = Dict \\ maxNat @(Eval x) @(Eval y) \\ summon @x \\ summon @y

instance (Summon x, Summon y) => Summon (NGcd x y) where
  summon = Dict \\ gcdNat @(Eval x) @(Eval y) \\ summon @x \\ summon @y

instance (Summon x, Summon y) => Summon (NLcm x y) where
  summon = Dict \\ lcmNat @(Eval x) @(Eval y) \\ summon @x \\ summon @y

instance (Summon x, Summon y, 1 <= Eval y) => Summon (x :/ y) where
  summon = Dict \\ divNat @(Eval x) @(Eval y) \\ summon @x \\ summon @y

instance (Summon x, Summon y, 1 <= Eval y) => Summon (x :% y) where
  summon = Dict \\ modNat @(Eval x) @(Eval y) \\ summon @x \\ summon @y

instance (Summon x, 1 <= Eval x) => Summon (NLog2 x) where
  summon = Dict \\ log2Nat @(Eval x) \\ summon @x
