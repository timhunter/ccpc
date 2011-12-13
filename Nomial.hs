module Nomial (Nomial, con, var, eval, subst) where

import Data.Bifunctor

data Nomial v a = Con a | Non (Nomial v a) v (Nomial v a)
  deriving (Eq, Ord)

instance (Show v, Show a) => Show (Nomial v a) where
  showsPrec d (Con x) = showsPrec d x
  showsPrec d (Non a u m) = showParen (d > 6) showStr
    where showStr = showsPrec 6 a . showString " + "
                  . showsPrec 7 u . showString " * " . showsPrec 8 m

instance Functor (Nomial v) where
  fmap g (Con x) = Con (g x)
  fmap g (Non a u m) = Non (fmap g a) u (fmap g m)

instance Bifunctor Nomial where
  bimap f g (Con x) = Con (g x)
  bimap f g (Non a u m) = Non (bimap f g a) (f u) (bimap f g m)

con :: a -> Nomial v a
con = Con

var :: (Num a) => v -> Nomial v a
var v = Non (Con 0) v (Con 1)

eval :: (Num a) => Nomial a a -> a
eval (Con x) = x
eval (Non a u m) = eval a + u * eval m

subst :: (Num a) => (v -> a) -> Nomial v a -> a
subst f = eval . first f

instance (Ord v, Show v, Num a) => Num (Nomial v a) where

  Con x + Con y = Con (x + y)
  x@(Non a u m) + y@(Non b v n) =
    case compare u v of LT -> Non (x + b) v n
                        EQ -> Non (a + b) u (m + n)
                        GT -> Non (a + y) u m
  x + Non b v n = Non (x + b) v n
  Non a u m + y = Non (a + y) u m

  Con x - Con y = Con (x - y)
  x@(Non a u m) - y@(Non b v n) =
    case compare u v of LT -> Non (x - b) v (-n)
                        EQ -> Non (a - b) u (m - n)
                        GT -> Non (a - y) u m
  x - Non b v n = Non (x - b) v (-n)
  Non a u m - y = Non (a - y) u m

  Con x * Con y = Con (x * y)
  x@(Non a u m) * y@(Non b v n) =
    case compare u v of LT -> Non (x * b) v (x * n)
                        EQ -> Non (a * b) u (a * n + m * y)
                        GT -> Non (a * y) u (m * y)
  x@(Con 0) * _ = x
  _ * y@(Con 0) = y
  x * Non b v n = Non (x * b) v (x * n)
  Non a u m * y = Non (a * y) u (m * y)

  negate = fmap negate
  abs = undefined
  signum = undefined
  fromInteger = Con . fromInteger
