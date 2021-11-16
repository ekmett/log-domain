{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013-2015
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Numeric.Log.Signed
( SignedLog(..)
) where

import Data.Data (Data(..))
import GHC.Generics (Generic(..))
import Numeric
import Text.Read as T
import Text.Show as T

-- | @Log@-domain @Float@ and @Double@ values, with a sign bit.
data SignedLog a = SLExp { signSL :: Bool, lnSL :: a} deriving (Data, Generic)

negInf :: Fractional a => a
negInf = (-1)/0

nan :: Fractional a => a
nan = 0/0

multSign :: (Num a) => Bool -> a -> a
multSign True = id
multSign False = (*) (-1)

-- $SignedLogCompTests
--
-- >>> (-7) < (3 :: SignedLog Double)
-- True
--
-- >>> 0 == (0 :: SignedLog Double)
-- True

instance (Eq a, Fractional a) => Eq (SignedLog a) where
  (SLExp sA a) == (SLExp sB b) = (a == b) && (sA == sB || a == negInf)

-- Does not necissarily handle NaNs in the same way as 'a' for >=, etc.
instance (Ord a, Fractional a) => Ord (SignedLog a) where
  compare (SLExp _ a) (SLExp _ b) | a == b && a == negInf = EQ
  compare (SLExp sA a) (SLExp sB b) = mappend (compare sA sB) $ compare a b

-- $SignedLogShowTests
--
-- >>> show (-0 :: SignedLog Double)
-- "0.0"
--
-- >>> show (1 :: SignedLog Double)
-- "1.0"
--
-- >>> show (-1 :: SignedLog Double)
-- "-1.0"

instance (Show a, RealFloat a, Eq a, Fractional a) => Show (SignedLog a) where
  showsPrec d (SLExp s a) = (if not s && a /= negInf && not (isNaN a) then T.showChar '-' else id) . T.showsPrec d (exp a)

instance (RealFloat a, Read a) => Read (SignedLog a) where
  readPrec = (realToFrac :: a -> SignedLog a) <$> step T.readPrec

nxor :: Bool -> Bool -> Bool
nxor = (==)

-- $SignedLogNumTests
--
-- Repeating internals, add testing function (~=)
--
-- >>> let nxor = (==)
-- >>> let multSign b = if b then id else (*) (-1)
--
-- >>> let SLExp sX x ~= SLExp sY y = abs ((exp x-(multSign (nxor sX sY) (exp y))) / exp x) < 0.01
--
-- Subtraction
--
-- >>> (3 - 1 :: SignedLog Double) ~= 2
-- True
--
-- >>> (1 - 3 :: SignedLog Double) ~= (-2)
-- True
--
-- >>> (3 - 2 :: SignedLog Float) ~= 1
-- True
--
-- >>> (1 - 3 :: SignedLog Float) ~= (-2)
-- True
--
-- >>> SLExp True (1/0) - SLExp True (1/0) :: SignedLog Double
-- NaN
--
-- >>> 0 - 0 :: SignedLog Double
-- 0.0
--
-- >>> 0 - SLExp True (1/0) :: SignedLog Double
-- -Infinity
--
-- >>> SLExp True (1/0) - 0.0 :: SignedLog Double
-- Infinity
--
-- Multiplication
--
-- >>> (3 * 2 :: SignedLog Double) ~= 6
-- True
--
-- >>> 0 * SLExp True (1/0) :: SignedLog Double
-- NaN
--
-- >>> SLExp True (1/0) * SLExp True (1/0) :: SignedLog Double
-- Infinity
--
-- >>> 0 * 0 :: SignedLog Double
-- 0.0
--
-- >>> SLExp True (0/0) * 0 :: SignedLog Double
-- NaN
--
-- >>> SLExp True (0/0) * SLExp True (1/0) :: SignedLog Double
-- NaN
--
-- Addition
--
-- >>> (3 + 1 :: SignedLog Double) ~= 4
-- True
--
-- >>> 0 + 0 :: SignedLog Double
-- 0.0
--
-- >>> SLExp True (1/0) + SLExp True (1/0) :: SignedLog Double
-- Infinity
--
-- >>> SLExp True (1/0) + 0 :: SignedLog Double
-- Infinity
--
-- Division
--
-- >>> (3 / 2 :: SignedLog Double) ~= 1.5
-- True
--
-- >>> 3 / 0 :: SignedLog Double
-- Infinity
--
-- >>> SLExp True (1/0) / 0 :: SignedLog Double
-- Infinity
--
-- >>> 0 / SLExp True (1/0) :: SignedLog Double
-- 0.0
--
-- >>> SLExp True (1/0) / SLExp True (1/0) :: SignedLog Double
-- NaN
--
-- >>> 0 / 0 :: SignedLog Double
-- NaN
--
-- Negation
--
-- >>> ((-3) + 8 :: SignedLog Double) ~= 8
-- False
--
-- >>> (-0) :: SignedLog Double
-- 0.0
--
-- >>> (-(0/0)) :: SignedLog Double
-- NaN
--
-- Signum
--
-- >>> signum 0 :: SignedLog Double
-- 0.0
--
-- >>> signum 3 :: SignedLog Double
-- 1.0
--
-- >>> signum (SLExp True (0/0)) :: SignedLog Double
-- NaN

instance RealFloat a => Num (SignedLog a) where
  SLExp sA a * SLExp sB b = SLExp (nxor sA sB) (a+b)
  {-# INLINE (*) #-}
  SLExp sA a + SLExp sB b
    | a == b && isInfinite a && (a < 0 || nxor sA sB) = SLExp True a
    | sA == sB && a >= b     = SLExp sA (a + log1pexp (b - a))
    | sA == sB && otherwise  = SLExp sA (b + log1pexp (a - b))
    | sA /= sB && a == b && not (isInfinite a) = SLExp True negInf
    | sA /= sB && a > b      = SLExp sA (a + log1mexp (b - a))
    | otherwise              = SLExp sB (b + log1mexp (a - b))
  {-# INLINE (+) #-}
  abs (SLExp _ a) = SLExp True a
  {-# INLINE abs #-}
  signum (SLExp sA a)
    | isInfinite a && a < 0 = SLExp True negInf
    | isNaN a = SLExp True nan -- signum(0/0::Double) == -1.0, this doesn't seem like a behavior worth replicating.
    | otherwise = SLExp sA 0
  {-# INLINE signum #-}
  fromInteger i = SLExp (i >= 0) $ log $ fromInteger $ abs i
  {-# INLINE fromInteger #-}
  negate (SLExp sA a) = SLExp (not sA) a
  {-# INLINE negate #-}

instance RealFloat a => Fractional (SignedLog a) where
  SLExp sA a / SLExp sB b = SLExp (nxor sA sB) (a-b)
  {-# INLINE (/) #-}
  fromRational a = SLExp (a >= 0) $ log $ fromRational $ abs a
  {-# INLINE fromRational #-}

-- $SignedLogToRationalTest
--
-- >>> (toRational (-3.5 :: SignedLog Double))
-- (-7) % 2

instance (RealFloat a, Ord a) => Real (SignedLog a) where
  toRational (SLExp sA a) = toRational $ multSign sA $ exp a
  {-# INLINE toRational #-}

logMap :: (Floating a, Ord a) => (a -> a) -> SignedLog a -> SignedLog a
logMap f (SLExp sA a) = SLExp (value >= 0) $ log $ abs value
  where value = f $ multSign sA $ exp a
{-# INLINE logMap #-}

instance RealFloat a => Floating (SignedLog a) where
  pi = SLExp True (log pi)
  {-# INLINE pi #-}
  exp (SLExp sA a) = SLExp True (multSign sA $ exp a)
  {-# INLINE exp #-}
  log (SLExp True a) = SLExp (a >= 0) (log $ abs a)
  log (SLExp False _) = nan
  {-# INLINE log #-}
  (SLExp sB b) ** (SLExp sE e) | sB || e == 0 || isInfinite e = SLExp sB (b * multSign sE (exp e))
  _ ** _ = nan
  {-# INLINE (**) #-}
  sqrt (SLExp True a) = SLExp True (a / 2)
  sqrt (SLExp False _) = nan
  {-# INLINE sqrt #-}
  logBase slA@(SLExp _ a) slB@(SLExp _ b) | slA >= 0 && slB >= 0 = SLExp (value >= 0) (log $ abs value)
    where value = logBase (exp a) (exp b)
  logBase _ _ = nan
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

-- $SignedLogProperFractionTests
--
-- >>> (properFraction (-1.5) :: (Integer, SignedLog Double))
-- (-1,-0.5)
--
-- >>> (properFraction (-0.5) :: (Integer, SignedLog Double))
-- (0,-0.5)

instance RealFloat a => RealFrac (SignedLog a) where
  properFraction slX@(SLExp sX x)
    | x < 0     = (0, slX)
    | otherwise = case properFraction $ multSign sX $ exp x of
      (b,a) -> (b, SLExp sX $ log $ abs a)
