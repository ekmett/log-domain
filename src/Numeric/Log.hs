{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Numeric.Log
  ( Log(..)
  , Precise(..)
  , sum
  ) where

import Prelude hiding (maximum, sum)
import Control.Applicative
import Control.Comonad
import Control.DeepSeq
import Data.Binary as Binary
import Data.Complex
import Data.Data
import Data.Distributive
import Data.Foldable as Foldable hiding (sum)
import Data.Functor.Bind
import Data.Functor.Extend
import Data.Hashable
import Data.Int
import Data.List as List hiding (sum)
import Data.Monoid
import Data.SafeCopy
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Traversable
import Foreign.Ptr
import Foreign.Storable
import Generics.Deriving
import Text.Read

-- | @Log@-domain @Float@ and @Double@ values.
newtype Log a = Log { runLog :: a } deriving (Eq,Ord,Data,Typeable,Generic)

instance (Floating a, Show a) => Show (Log a) where
  showsPrec d (Log a) = showsPrec d (exp a)

instance (Floating a, Read a) => Read (Log a) where
  readPrec = Log . log <$> step readPrec

instance Binary a => Binary (Log a) where
  put = put . runLog
  {-# INLINE put #-}
  get = Log <$> Binary.get
  {-# INLINE get #-}

instance Functor Log where
  fmap f (Log a) = Log (f a)
  {-# INLINE fmap #-}

instance Hashable a => Hashable (Log a) where
  hashWithSalt i (Log a) = hashWithSalt i a
  {-# INLINE hashWithSalt #-}

instance Storable a => Storable (Log a) where
  sizeOf = sizeOf . runLog
  {-# INLINE sizeOf #-}
  alignment = alignment . runLog
  {-# INLINE alignment #-}
  peek ptr = Log <$> peek (castPtr ptr)
  {-# INLINE peek #-}
  poke ptr (Log a) = poke (castPtr ptr) a
  {-# INLINE poke #-}

instance NFData a => NFData (Log a) where
  rnf (Log a) = rnf a
  {-# INLINE rnf #-}

instance Foldable Log where
  foldMap f (Log a) = f a
  {-# INLINE foldMap #-}

instance Foldable1 Log where
  foldMap1 f (Log a) = f a
  {-# INLINE foldMap1 #-}

instance Traversable Log where
  traverse f (Log a) = Log <$> f a
  {-# INLINE traverse #-}

instance Traversable1 Log where
  traverse1 f (Log a) = Log <$> f a
  {-# INLINE traverse1 #-}

instance Distributive Log where
  distribute = Log . fmap runLog
  {-# INLINE distribute #-}

instance Extend Log where
  extended f w@Log{} = Log (f w)
  {-# INLINE extended #-}

instance Comonad Log where
  extract (Log a) = a
  {-# INLINE extract #-}
  extend f w@Log{} = Log (f w)
  {-# INLINE extend #-}

instance Applicative Log where
  pure = Log
  {-# INLINE pure #-}
  Log f <*> Log a = Log (f a)
  {-# INLINE (<*>) #-}

instance ComonadApply Log where
  Log f <@> Log a = Log (f a)
  {-# INLINE (<@>) #-}

instance Apply Log where
  Log f <.> Log a = Log (f a)
  {-# INLINE (<.>) #-}

instance Bind Log where
  Log a >>- f = f a
  {-# INLINE (>>-) #-}

instance Monad Log where
  return = Log
  {-# INLINE return #-}
  Log a >>= f = f a
  {-# INLINE (>>=) #-}

deriveSafeCopy 1 'base ''Log

-- | Negative infinity
negInf :: Fractional a => a
negInf = -(1/0)
{-# INLINE negInf #-}

instance (Precise a, RealFloat a) => Num (Log a) where
  Log a * Log b
    | isInfinite a && isInfinite b && a == -b = Log negInf
    | otherwise = Log (a + b)
  {-# INLINE (*) #-}
  Log a + Log b
    | a == b && isInfinite a && isInfinite b = Log a
    | a >= b    = Log (a + log1p (exp (b - a)))
    | otherwise = Log (b + log1p (exp (a - b)))
  {-# INLINE (+) #-}
  Log a - Log b
    | a == negInf && b == negInf = Log negInf
    | otherwise = Log (a + log1p (negate (exp (b - a))))
  {-# INLINE (-) #-}
  signum (Log a)
    | a == negInf = 0
    | a > negInf  = 1
    | otherwise   = negInf
  {-# INLINE signum #-}
  negate _ = negInf
  {-# INLINE negate #-}
  abs = id
  {-# INLINE abs #-}
  fromInteger = Log . log . fromInteger
  {-# INLINE fromInteger #-}

instance (Precise a, RealFloat a, Eq a) => Fractional (Log a) where
  -- n/0 == infinity is handled seamlessly for us. We must catch 0/0 and infinity/infinity NaNs, and handle 0/infinity.
  Log a / Log b
    | a == b && isInfinite a && isInfinite b = Log negInf
    | a == negInf                            = Log negInf
    | otherwise                              = Log (a-b)
  {-# INLINE (/) #-}
  fromRational = Log . log . fromRational
  {-# INLINE fromRational #-}

instance (Precise a, RealFloat a, Ord a) => Real (Log a) where
  toRational (Log a) = toRational (exp a)
  {-# INLINE toRational #-}

data Acc1 a = Acc1 {-# UNPACK #-} !Int64 !a

instance (Precise a, RealFloat a) => Monoid (Log a) where
  mempty  = Log negInf
  {-# INLINE mempty #-}
  mappend = (+)
  {-# INLINE mappend #-}
  mconcat [] = 0
  mconcat (Log z:zs) = Log $ case List.foldl' step1 (Acc1 0 z) zs of
    Acc1 nm1 a
      | isInfinite a -> a
      | otherwise    -> a + log1p (List.foldl' (step2 a) 0 zs + fromIntegral nm1)
    where
      step1 (Acc1 n y) (Log x) = Acc1 (n + 1) (max x y)
      step2 a r (Log x) = r + expm1 (x - a)
  {-# INLINE mconcat #-}

logMap :: Floating a => (a -> a) -> Log a -> Log a
logMap f = Log . log . f . exp . runLog
{-# INLINE logMap #-}

data Acc a = Acc {-# UNPACK #-} !Int64 !a | None

-- | Efficiently and accurately compute the sum of a set of log-domain numbers
--
-- While folding with @(+)@ accomplishes the same end, it requires an
-- additional @n-2@ logarithms to sum @n@ terms. In addition,
-- here we introduce fewer opportunities for round-off error.
--
-- While for small quantities the naive sum accumulates error,
--
-- >>> let xs = replicate 40000 (Log 1e-4) :: [Log Float]
-- >>> Prelude.sum xs
-- 40001.3
--
-- This sum gives a more accurate result,
--
-- >>> Numeric.Log.sum xs
-- 40004.01
--
-- /NB:/ This does require two passes over the data.
sum :: (RealFloat a, Ord a, Precise a, Foldable f) => f (Log a) -> Log a
sum xs = Log $ case Foldable.foldl' step1 None xs of
  None -> negInf
  Acc nm1 a
    | isInfinite a -> a
    | otherwise    -> a + log1p (Foldable.foldl' (step2 a) 0 xs + fromIntegral nm1)
  where
    step1 None      (Log x) = Acc 0 x
    step1 (Acc n y) (Log x) = Acc (n + 1) (max x y)
    step2 a r (Log x) = r + expm1 (x - a)
{-# INLINE sum #-}

instance (RealFloat a, Precise a) => Floating (Log a) where
  pi = Log (log pi)
  {-# INLINE pi #-}
  exp (Log a) = Log (exp a)
  {-# INLINE exp #-}
  log (Log a) = Log (log a)
  {-# INLINE log #-}
  sqrt (Log a) = Log (a / 2)
  {-# INLINE sqrt #-}
  logBase (Log a) (Log b) = Log (log (logBase (exp a) (exp b)))
  {-# INLINE logBase #-}
  sin = logMap sin
  {-# INLINE sin #-}
  cos = logMap cos
  {-# INLINE cos #-}
  tan = logMap tan
  {-# INLINE tan #-}
  asin = logMap asin
  {-# INLINE asin #-}
  acos = logMap acos
  {-# INLINE acos #-}
  atan = logMap atan
  {-# INLINE atan #-}
  sinh = logMap sinh
  {-# INLINE sinh #-}
  cosh = logMap cosh
  {-# INLINE cosh #-}
  tanh = logMap tanh
  {-# INLINE tanh #-}
  asinh = logMap asinh
  {-# INLINE asinh #-}
  acosh = logMap acosh
  {-# INLINE acosh #-}
  atanh = logMap atanh
  {-# INLINE atanh #-}

{-# RULES
"realToFrac" realToFrac = Log . realToFrac . runLog :: Log Double -> Log Float
"realToFrac" realToFrac = Log . realToFrac . runLog :: Log Float -> Log Double
"realToFrac" realToFrac = exp . runLog :: Log Double -> Double
"realToFrac" realToFrac = exp . runLog :: Log Float -> Float
"realToFrac" realToFrac = Log . log :: Double -> Log Double
"realToFrac" realToFrac = Log . log :: Float -> Log Float #-}

-- | This provides @log1p@ and @expm1@ for working more accurately with small numbers.
class Floating a => Precise a where
  -- | Computes @log(1 + x)@
  --
  -- This is far enough from 0 that the Taylor series is defined.
  log1p :: a -> a

  -- | The Taylor series for exp(x) is given by
  --
  -- > exp(x) = 1 + x + x^2/2! + ...
  --
  -- When @x@ is small, the leading 1 consumes all of the available precision.
  --
  -- This computes:
  --
  -- > exp(x) - 1 = x + x^2/2! + ..
  --
  -- which can afford you a great deal of additional precision if you move things around
  -- algebraically to provide the 1 by other means.
  expm1 :: a -> a

instance Precise Double where
  log1p = c_log1p
  {-# INLINE log1p #-}
  expm1 = c_expm1
  {-# INLINE expm1 #-}

instance Precise Float where
  log1p = c_log1pf
  {-# INLINE log1p #-}
  expm1 = c_expm1f
  {-# INLINE expm1 #-}

instance (RealFloat a, Precise a) => Precise (Complex a) where
  expm1 x@(a :+ b)
    | a*a + b*b < 1, u <- expm1 a, v <- sin (b/2), w <- -2*v*v = (u*w+u+w) :+ (u+1)*sin b
    | otherwise = exp x - 1
  {-# INLINE expm1 #-}
  log1p x@(a :+ b)
    | abs a < 0.5 && abs b < 0.5, u <- 2*a+a*a+b*b = log1p (u/(1+sqrt (u+1))) :+ atan2 (1 + a) b
    | otherwise = log (1 + x)
  {-# INLINE log1p #-}

foreign import ccall unsafe "math.h log1p" c_log1p :: Double -> Double
foreign import ccall unsafe "math.h expm1" c_expm1 :: Double -> Double
foreign import ccall unsafe "math.h expm1f" c_expm1f :: Float -> Float
foreign import ccall unsafe "math.h log1pf" c_log1pf :: Float -> Float
