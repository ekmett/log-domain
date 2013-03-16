{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
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
  ) where

import Control.Applicative
import Control.Comonad
import Control.DeepSeq
import Data.Binary as Binary
import Data.Complex
import Data.Distributive
import Data.Foldable
import Data.Functor.Apply
import Data.Hashable
import Data.Traversable
import Data.Data
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

instance Traversable Log where
  traverse f (Log a) = Log <$> f a
  {-# INLINE traverse #-}

instance Distributive Log where
  distribute = Log . fmap runLog
  {-# INLINE distribute #-}

instance Comonad Log where
  extract (Log a) = a
  {-# INLINE extract #-}
  extend f w@(Log _) = Log (f w)
  {-# INLINE extend #-}

instance Applicative Log where
  pure = Log
  {-# INLINE pure #-}
  Log f <*> Log a = Log (f a)
  {-# INLINE (<*>) #-}

instance Apply Log where
  Log f <.> Log a = Log (f a)
  {-# INLINE (<.>) #-}

instance Monad Log where
  return = Log
  {-# INLINE return #-}
  Log a >>= f = f a
  {-# INLINE (>>=) #-}

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
