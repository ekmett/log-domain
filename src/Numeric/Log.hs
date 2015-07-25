{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Trustworthy #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013-2015
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
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Comonad
import Control.DeepSeq
import Control.Monad
import Data.Binary as Binary
import Data.Bytes.Serial
import Data.Complex
import Data.Data
import Data.Distributive
import Data.Foldable as Foldable hiding (sum)
import Data.Functor.Bind
import Data.Functor.Extend
import Data.Hashable
import Data.Hashable.Extras
import Data.Int
import Data.List as List hiding (sum)
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Data.SafeCopy
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Serialize as Serialize
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif
import Data.Vector.Unboxed as U hiding (sum)
import Data.Vector.Generic as G hiding (sum)
import Data.Vector.Generic.Mutable as M
import Foreign.Ptr
import Foreign.Storable
#if __GLASGOW_HASKELL__ < 706
import Generics.Deriving
#else
import GHC.Generics
#endif
import Text.Read as T
import Text.Show as T

{-# ANN module "HLint: ignore Eta reduce" #-}

-- $setup
-- >>> let Exp x ~= Exp y = abs ((exp x-exp y) / exp x) < 0.01

-- | @Log@-domain @Float@ and @Double@ values.
newtype Log a = Exp { ln :: a } deriving (Eq,Ord,Data,Typeable,Generic)

deriveSafeCopy 1 'base ''Log

instance (Floating a, Show a) => Show (Log a) where
  showsPrec d (Exp a) = T.showsPrec d (exp a)

instance (Floating a, Read a) => Read (Log a) where
  readPrec = Exp . log <$> step T.readPrec

instance Binary a => Binary (Log a) where
  put = Binary.put . ln
  {-# INLINE put #-}
  get = Exp <$> Binary.get
  {-# INLINE get #-}

instance Serialize a => Serialize (Log a) where
  put = Serialize.put . ln
  {-# INLINE put #-}
  get = Exp <$> Serialize.get
  {-# INLINE get #-}

instance Serial a => Serial (Log a) where
  serialize = serialize . ln
  deserialize = Exp <$> deserialize

instance Serial1 Log where
  serializeWith f = f . ln
  deserializeWith m = Exp <$> m

instance Functor Log where
  fmap f (Exp a) = Exp (f a)
  {-# INLINE fmap #-}

instance Hashable a => Hashable (Log a) where
  hashWithSalt i (Exp a) = hashWithSalt i a
  {-# INLINE hashWithSalt #-}

instance Hashable1 Log

instance Storable a => Storable (Log a) where
  sizeOf = sizeOf . ln
  {-# INLINE sizeOf #-}
  alignment = alignment . ln
  {-# INLINE alignment #-}
  peek ptr = Exp <$> peek (castPtr ptr)
  {-# INLINE peek #-}
  poke ptr (Exp a) = poke (castPtr ptr) a
  {-# INLINE poke #-}

instance NFData a => NFData (Log a) where
  rnf (Exp a) = rnf a
  {-# INLINE rnf #-}

instance Foldable Log where
  foldMap f (Exp a) = f a
  {-# INLINE foldMap #-}

instance Foldable1 Log where
  foldMap1 f (Exp a) = f a
  {-# INLINE foldMap1 #-}

instance Traversable Log where
  traverse f (Exp a) = Exp <$> f a
  {-# INLINE traverse #-}

instance Traversable1 Log where
  traverse1 f (Exp a) = Exp <$> f a
  {-# INLINE traverse1 #-}

instance Distributive Log where
  distribute = Exp . fmap ln
  {-# INLINE distribute #-}

instance Extend Log where
  extended f w@Exp{} = Exp (f w)
  {-# INLINE extended #-}

instance Comonad Log where
  extract (Exp a) = a
  {-# INLINE extract #-}
  extend f w@Exp{} = Exp (f w)
  {-# INLINE extend #-}

instance Applicative Log where
  pure = Exp
  {-# INLINE pure #-}
  Exp f <*> Exp a = Exp (f a)
  {-# INLINE (<*>) #-}

instance ComonadApply Log where
  Exp f <@> Exp a = Exp (f a)
  {-# INLINE (<@>) #-}

instance Apply Log where
  Exp f <.> Exp a = Exp (f a)
  {-# INLINE (<.>) #-}

instance Bind Log where
  Exp a >>- f = f a
  {-# INLINE (>>-) #-}

instance Monad Log where
  return = Exp
  {-# INLINE return #-}
  Exp a >>= f = f a
  {-# INLINE (>>=) #-}

instance (RealFloat a, Precise a, Enum a) => Enum (Log a) where
  succ a = a + 1
  {-# INLINE succ #-}
  pred a = a - 1
  {-# INLINE pred #-}
  toEnum   = fromIntegral
  {-# INLINE toEnum #-}
  fromEnum = round . exp . ln
  {-# INLINE fromEnum #-}
  enumFrom (Exp a) = [ Exp (log b) | b <- Prelude.enumFrom (exp a) ]
  {-# INLINE enumFrom #-}
  enumFromThen (Exp a) (Exp b) = [ Exp (log c) | c <- Prelude.enumFromThen (exp a) (exp b) ]
  {-# INLINE enumFromThen #-}
  enumFromTo (Exp a) (Exp b) = [ Exp (log c) | c <- Prelude.enumFromTo (exp a) (exp b) ]
  {-# INLINE enumFromTo #-}
  enumFromThenTo (Exp a) (Exp b) (Exp c) = [ Exp (log d) | d <- Prelude.enumFromThenTo (exp a) (exp b) (exp c) ]
  {-# INLINE enumFromThenTo #-}

-- | Negative infinity
negInf :: Fractional a => a
negInf = -(1/0)
{-# INLINE negInf #-}

-- $LogNumTests
--
-- Subtraction
--
-- >>> (3 - 1 :: Log Double) ~= 2
-- True
--
-- >>> 1 - 3 :: Log Double
-- NaN
-- 
-- >>> (3 - 2 :: Log Float) ~= 1
-- True
--
-- >>> 1 - 3 :: Log Float
-- NaN
--
-- >>> (Exp (1/0)) - (Exp (1/0)) :: Log Double
-- NaN
--
-- >>> 0 - 0 :: Log Double
-- 0.0
--
-- >>> 0 - (Exp (1/0)) :: Log Double
-- NaN
--
-- >>> (Exp (1/0)) - 0.0 :: Log Double
-- Infinity
--
-- Multiplication
--
-- >>> (3 * 2 :: Log Double) ~= 6
-- True
--
-- >>> 0 * (Exp (1/0)) :: Log Double
-- NaN
--
-- >>> (Exp (1/0)) * (Exp (1/0)) :: Log Double
-- Infinity
--
-- >>> 0 * 0 :: Log Double
-- 0.0
--
-- >>> (Exp (0/0)) * 0 :: Log Double
-- NaN
--
-- >>> (Exp (0/0)) * (Exp (1/0)) :: Log Double
-- NaN
--
-- Addition
--
-- >>> (3 + 1 :: Log Double) ~= 4
-- True
--
-- >>> 0 + 0 :: Log Double
-- 0.0
--
-- >>> (Exp (1/0)) + (Exp (1/0)) :: Log Double
-- Infinity
--
-- >>> (Exp (1/0)) + 0 :: Log Double
-- Infinity
--
-- Division
--
-- >>> (3 / 2 :: Log Double) ~= 1.5
-- True
--
-- >>> 3 / 0 :: Log Double
-- Infinity
--
-- >>> (Exp (1/0)) / 0 :: Log Double
-- Infinity
--
-- >>> 0 / (Exp (1/0)) :: Log Double
-- 0.0
--
-- >>> (Exp (1/0)) / (Exp (1/0)) :: Log Double
-- NaN
--
-- >>> 0 / 0 :: Log Double
-- NaN
--
-- Negation
--
-- >>> ((-3) + 8 :: Log Double) ~= 8
-- False
--
-- >>> (-0) :: Log Double
-- 0.0
--
-- >>> (-(0/0)) :: Log Double
-- NaN
--
-- Signum
--
-- >>> signum 0 :: Log Double
-- 0.0
--
-- >>> signum 3 :: Log Double
-- 1.0
--
-- >>> signum (Exp (0/0)) :: Log Double
-- NaN

instance (Precise a, RealFloat a) => Num (Log a) where
  Exp a * Exp b = Exp (a + b)
  {-# INLINE (*) #-}
  Exp a + Exp b
    | a == b && isInfinite a && isInfinite b = Exp a
    | a >= b    = Exp (a + log1pexp (b - a))
    | otherwise = Exp (b + log1pexp (a - b))
  {-# INLINE (+) #-}
  Exp a - Exp b
    | isInfinite a && isInfinite b && a < 0 && b < 0 = Exp negInf
    | otherwise = Exp (a + log1mexp (b - a))
  {-# INLINE (-) #-}
  signum a
    | a == 0    = Exp negInf -- 0
    | a > 0     = Exp 0      -- 1
    | otherwise = Exp (0/0)  -- NaN
  {-# INLINE signum #-}
  negate (Exp a)
    | isInfinite a && a < 0 = Exp negInf
    | otherwise             = Exp (0/0)
  {-# INLINE negate #-}
  abs = id
  {-# INLINE abs #-}
  fromInteger = Exp . log . fromInteger
  {-# INLINE fromInteger #-}

instance (Precise a, RealFloat a, Eq a) => Fractional (Log a) where
  -- n/0 == infinity is handled seamlessly for us, as is 0/0 and infinity/infinity NaNs, and 0/infinity == 0.
  Exp a / Exp b = Exp (a-b)
  {-# INLINE (/) #-}
  fromRational = Exp . log . fromRational
  {-# INLINE fromRational #-}

-- $LogProperFractionTests
--
-- >>> (properFraction 3.5 :: (Integer, Log Double))
-- (3,0.5)
--
-- >>> (properFraction 0.5 :: (Integer, Log Double))
-- (0,0.5)

instance (Precise a, RealFloat a) => RealFrac (Log a) where
  properFraction l
    | ln l < 0  = (0, l)
    | otherwise = (\(b,a) -> (b, Exp $ log a)) $ properFraction $ exp (ln l)

newtype instance U.MVector s (Log a) = MV_Log (U.MVector s a)
newtype instance U.Vector    (Log a) = V_Log  (U.Vector    a)

instance (RealFloat a, Unbox a) => Unbox (Log a)

instance (RealFloat a, Unbox a) => M.MVector U.MVector (Log a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Log v) = M.basicLength v
  basicUnsafeSlice i n (MV_Log v) = MV_Log $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Log v1) (MV_Log v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Log `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (Exp x) = MV_Log `liftM` M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_Log v) i = Exp `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Log v) i (Exp x) = M.basicUnsafeWrite v i x
  basicClear (MV_Log v) = M.basicClear v
  basicSet (MV_Log v) (Exp x) = M.basicSet v x
  basicUnsafeCopy (MV_Log v1) (MV_Log v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_Log v) n = MV_Log `liftM` M.basicUnsafeGrow v n

instance (RealFloat a, Unbox a) => G.Vector U.Vector (Log a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Log v) = V_Log `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Log v) = MV_Log `liftM` G.basicUnsafeThaw v
  basicLength (V_Log v) = G.basicLength v
  basicUnsafeSlice i n (V_Log v) = V_Log $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Log v) i = Exp `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Log mv) (V_Log v) = G.basicUnsafeCopy mv v
  elemseq _ (Exp x) z = G.elemseq (undefined :: U.Vector a) x z

instance (Precise a, RealFloat a, Ord a) => Real (Log a) where
  toRational (Exp a) = toRational (exp a)
  {-# INLINE toRational #-}

data Acc1 a = Acc1 {-# UNPACK #-} !Int64 !a

instance (Precise a, RealFloat a) => Monoid (Log a) where
  mempty  = Exp negInf
  {-# INLINE mempty #-}
  mappend = (+)
  {-# INLINE mappend #-}
  mconcat [] = 0
  mconcat (Exp z:zs) = Exp $ case List.foldl' step1 (Acc1 0 z) zs of
    Acc1 nm1 a
      | isInfinite a -> a
      | otherwise    -> a + log1p (List.foldl' (step2 a) 0 zs + fromIntegral nm1)
    where
      step1 (Acc1 n y) (Exp x) = Acc1 (n + 1) (max x y)
      step2 a r (Exp x) = r + expm1 (x - a)
  {-# INLINE mconcat #-}

logMap :: Floating a => (a -> a) -> Log a -> Log a
logMap f = Exp . log . f . exp . ln
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
-- >>> let xs = Prelude.replicate 40000 (Exp 1e-4) :: [Log Float]
-- >>> Prelude.sum xs ~= 4.00e4
-- True
--
-- This sum gives a more accurate result,
--
-- >>> Numeric.Log.sum xs ~= 4.00e4
-- True
--
-- /NB:/ This does require two passes over the data.
sum :: (RealFloat a, Ord a, Precise a, Foldable f) => f (Log a) -> Log a
sum xs = Exp $ case Foldable.foldl' step1 None xs of
  None -> negInf
  Acc nm1 a
    | isInfinite a -> a
    | otherwise    -> a + log1p (Foldable.foldl' (step2 a) 0 xs + fromIntegral nm1)
  where
    step1 None      (Exp x) = Acc 0 x
    step1 (Acc n y) (Exp x) = Acc (n + 1) (max x y)
    step2 a r (Exp x) = r + expm1 (x - a)
{-# INLINE sum #-}

instance (RealFloat a, Precise a) => Floating (Log a) where
  pi = Exp (log pi)
  {-# INLINE pi #-}
  exp (Exp a) = Exp (exp a)
  {-# INLINE exp #-}
  log (Exp a) = Exp (log a)
  {-# INLINE log #-}
  Exp b ** Exp e = Exp (b * exp e)
  {-# INLINE (**) #-}
  sqrt (Exp a) = Exp (a / 2)
  {-# INLINE sqrt #-}
  logBase (Exp a) (Exp b) = Exp (log (logBase (exp a) (exp b)))
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
"realToFrac" realToFrac = Exp . realToFrac . ln :: Log Double -> Log Float
"realToFrac" realToFrac = Exp . realToFrac . ln :: Log Float -> Log Double
"realToFrac" realToFrac = exp . ln :: Log Double -> Double
"realToFrac" realToFrac = exp . ln :: Log Float -> Float
"realToFrac" realToFrac = Exp . log :: Double -> Log Double
"realToFrac" realToFrac = Exp . log :: Float -> Log Float #-}

-- | This provides @log1p@ and @expm1@ for working more accurately with small numbers.
class Floating a => Precise a where
  -- | Computes @log(1 + x)@
  --
  -- This is far enough from 0 that the Taylor series is defined.
  --
  -- This can provide much more accurate answers for logarithms of numbers close to 1 (x near 0).
  --
  -- These arise when working wth log-scale probabilities a lot.
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

  log1pexp :: a -> a
  log1pexp a = log1p (exp a)

  log1mexp :: a -> a
  log1mexp a = log1p (negate (exp a))

instance Precise Double where
  log1p = c_log1p
  {-# INLINE log1p #-}
  expm1 = c_expm1
  {-# INLINE expm1 #-}
  log1mexp a
    | a <= log 2 = log (negate (expm1 a))
    | otherwise  = log1p (negate (exp a))
  {-# INLINE log1mexp #-}
  log1pexp a
    | a <= 18   = log1p (exp a)
    | a <= 100  = a + exp (negate a)
    | otherwise = a
  {-# INLINE log1pexp #-}


instance Precise Float where
  log1p = c_log1pf
  {-# INLINE log1p #-}
  expm1 = c_expm1f
  {-# INLINE expm1 #-}
  log1mexp a | a <= log 2 = log (negate (expm1 a))
             | otherwise  = log1p (negate (exp a))
  {-# INLINE log1mexp #-}
  log1pexp a
    | a <= 18   = log1p (exp a)
    | a <= 100  = a + exp (negate a)
    | otherwise = a
  {-# INLINE log1pexp #-}

instance (RealFloat a, Precise a) => Precise (Complex a) where
  expm1 x@(a :+ b)
    | a*a + b*b < 1, u <- expm1 a, v <- sin (b/2), w <- -2*v*v = (u*w+u+w) :+ (u+1)*sin b
    | otherwise = exp x - 1
  {-# INLINE expm1 #-}
  log1p x@(a :+ b)
    | abs a < 0.5 && abs b < 0.5, u <- 2*a+a*a+b*b = log1p (u/(1+sqrt (u+1))) :+ atan2 (1 + a) b
    | otherwise = log (1 + x)
  {-# INLINE log1p #-}

#ifdef __USE_FFI__

foreign import ccall unsafe "math.h log1p" c_log1p :: Double -> Double
foreign import ccall unsafe "math.h expm1" c_expm1 :: Double -> Double
foreign import ccall unsafe "math.h expm1f" c_expm1f :: Float -> Float
foreign import ccall unsafe "math.h log1pf" c_log1pf :: Float -> Float

#else

c_log1p :: Double -> Double
{-# INLINE c_log1p #-}
c_log1p x = log (1 + x)

c_expm1 :: Double -> Double
{-# INLINE c_expm1 #-}
c_expm1 x = exp x - 1

c_expm1f :: Float -> Float
{-# INLINE c_expm1f #-}
c_expm1f x = exp x - 1

c_log1pf :: Float -> Float
{-# INLINE c_log1pf #-}
c_log1pf x = log (1 + x)

#endif
