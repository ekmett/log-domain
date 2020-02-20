{-# LANGUAGE CPP #-}
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

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (Monoid(..))
#endif
import Data.Data (Data(..))
import GHC.Generics (Generic(..))
import Data.Typeable (Typeable)
import Numeric
import Text.Read as T
import Text.Show as T
#if __GLASGOW_HASKELL__ < 710
import Data.Functor ((<$>))
#endif

-- $setup
-- >>> let SLExp sX x ~= SLExp sY y = abs ((exp x-(multSign (nxor sX sY) (exp y))) / exp x) < 0.01

-- | @Log@-domain @Float@ and @Double@ values, with a sign bit.
data SignedLog a = SLExp { signSL :: Bool, lnSL :: a} deriving (Data, Typeable, Generic)

negInf :: Fractional a => a
negInf = (-1)/0

nan :: Fractional a => a
nan = 0/0

-- | Machine epsilon, the difference between 1 and the next representable value
eps :: RealFloat a => a
eps = let ret = scaleFloat (1 - floatDigits ret) 1 in ret

multSign :: (Num a) => Bool -> a -> a
multSign True = id
multSign False = negate

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
-- "-0.0"
--
-- >>> show (1 :: SignedLog Double)
-- "1.0"
--
-- >>> show (-1 :: SignedLog Double)
-- "-1.0"

instance (Show a, RealFloat a, Eq a, Fractional a) => Show (SignedLog a) where
  showsPrec d (SLExp s a) = (if not s && not (isNaN a) then T.showChar '-' else id) . T.showsPrec d (exp a)

instance (RealFloat a, Read a) => Read (SignedLog a) where
  readPrec = (realToFrac :: a -> SignedLog a) <$> step T.readPrec

nxor :: Bool -> Bool -> Bool
nxor = (==)

-- $SignedLogNumTests
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
-- >>> (-0) + (-0) :: SignedLog Double
-- -0.0
--
-- >>> SLExp True (1/0) + SLExp True (1/0) :: SignedLog Double
-- Infinity
--
-- >>> SLExp False (1/0) + SLExp False (1/0) :: SignedLog Double
-- -Infinity
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
-- -0.0
--
-- >>> (-(0/0)) :: SignedLog Double
-- NaN
--
-- Signum
--
-- >>> signum 0 :: SignedLog Double
-- 0.0
--
-- >>> signum (-0) :: SignedLog Double
-- -0.0
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
    | a == b && isInfinite a && (a < 0 || nxor sA sB) = SLExp (sA || sB) a
    | sA == sB && a >= b     = SLExp sA (a + log1pexp (b - a))
    | sA == sB && otherwise  = SLExp sA (b + log1pexp (a - b))
    | sA /= sB && a == b && not (isInfinite a) = SLExp True negInf
    | sA /= sB && a > b      = SLExp sA (a + log1mexp (b - a))
    | otherwise              = SLExp sB (b + log1mexp (a - b))
  {-# INLINE (+) #-}
  abs (SLExp _ a) = SLExp True a
  {-# INLINE abs #-}
  signum (SLExp sA a)
    | isInfinite a && a < 0 = SLExp sA negInf
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

-- $SignedLogFloatingTests
--
-- >>> (sinh (SLExp True (-17)) :: SignedLog Double) ~= SLExp True (-17)
-- True
--
-- >>> (sinh (SLExp True (-18)) :: SignedLog Double) ~= SLExp True (-18)
-- True
--
-- >>> sinh 0 :: SignedLog Double
-- 0.0
--
-- >>> (sinh 1 :: SignedLog Double) ~= 1.1752
-- True
--
-- >>> (sinh (-1) :: SignedLog Double) == negate (sinh 1)
-- True
--
-- >>> floor (lnSL (sinh (SLExp True 12) :: SignedLog Double))
-- 162754
--
-- >>> cosh 0 :: SignedLog Double
-- 1.0
--
-- >>> (cosh 1 :: SignedLog Double) ~= 1.543
-- True
--
-- >>> (cosh (-1) :: SignedLog Double) == cosh 1
-- True
--
-- >>> floor (lnSL (cosh (SLExp True 12) :: SignedLog Double))
-- 162754
--
-- >>> (tanh (SLExp True (-17)) :: SignedLog Double) ~= SLExp True (-17)
-- True
--
-- >>> (tanh (SLExp True (-18)) :: SignedLog Double) ~= SLExp True (-18)
-- True
--
-- >>> tanh 0 :: SignedLog Double
-- 0.0
--
-- >>> (tanh 1 :: SignedLog Double) ~= (sinh 1 / cosh 1)
-- True
--
-- >>> (tanh (-1) :: SignedLog Double) == negate (tanh 1)
-- True
--
-- >>> tanh (SLExp True 12) :: SignedLog Double
-- 1.0
--
-- >>> (log1p 1 :: SignedLog Double) ~= log 2
-- True
--
-- >>> (log1p (-0.5) :: SignedLog Double) ~= log 0.5
-- True
--
-- >>> log (log1p (SLExp True (exp 100)) :: SignedLog Double) ~= 100
-- True

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
    where value = b / a
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

  -- log (sinh (exp a))
  --   = a + log (sinh (exp a) / exp a))
  --   = a + log (1 + (exp a)^2 / 6 + (exp a)^4 / 120 + ...)
  --   = a + (exp a)^2 / 6 - (exp a)^4 / 180 + ...
  -- a < log (sinh (exp a)) < a + (exp a)^2 / 6
  -- Therefore, if a < log (3 * eps) / 2,
  -- a < log (sinh (exp a)) < a + eps / 2
  -- and so if a < -1, then log (sinh (exp a)) rounds to a
  sinh (SLExp sA a) = SLExp sA logValue
    where expA = exp a
          logValue | a < min (-1) (log (3*eps) / 2) = a
                   | a < 0 = log (sinh expA)
                   | otherwise = expA + log ((1 - exp (-2 * expA)) / 2)
  {-# INLINE sinh #-}
  cosh (SLExp _ a) = SLExp True (expA + log ((1 + exp (-2 * expA)) / 2))
    where expA = exp a
  {-# INLINE cosh #-}
  -- log (tanh (exp a))
  --   = a - (exp a)^2 / 3 + 7 * (exp a)^4 / 90 - ...
  tanh (SLExp sA a) = SLExp sA logValue
    where logValue | a < min (-1) (log (3*eps/2) / 2) = a
                   | otherwise = log (tanh (exp a))
  {-# INLINE tanh #-}
  asinh = logMap asinh
  {-# INLINE asinh #-}
  acosh = logMap acosh
  {-# INLINE acosh #-}
  atanh = logMap atanh
  {-# INLINE atanh #-}

  log1p (SLExp True a) = SLExp True (log (log1pexp a))
  log1p (SLExp False a) = SLExp (a > 0) (log (negate (log1mexp a))) -- return positive NaN on failure
  {-# INLINE log1p #-}

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
