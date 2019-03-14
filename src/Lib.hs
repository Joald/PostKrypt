{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib
  ( R
  , R2
  , Vec
  , Point
  , point
  , vec
  , Picture(..)
  , line
  , rectangle
  , (&)
  , IntLine
  , IntRendering
  , renderScaled
  , Transform
  , translate
  , rotate
  , fullCircle
  , trpoint
  , trvec
  , transform
  -- also export some utils
  , pmap
  , trR2
  , Line
  ) where

import Control.Monad
import Data.Fixed (mod')
import GHC.IO (unsafePerformIO)
import GHC.Real
import Mon
import Prelude hiding (cos, sin)

-- | All the type definitions.
type R = Rational

type R2 = (R, R)

newtype Point = Point
  { getCoords :: R2
  } deriving (Show, Eq)

newtype Vec = Vec
  { getVec :: R2
  } deriving (Show, Eq)

newtype Transform =
  Trans (Vec, R)
  deriving (Show, Eq)

type Line = (R2, R2)

newtype Picture = Picture
  { getLineList :: [Line]
  } deriving (Show, Eq)

type IntLine = ((Int, Int), (Int, Int))

type IntRendering = [IntLine]

-- | All the instances.
instance Mon Vec where
  m1 = Vec (0, 0)
  x >< y = vec (fx + sx, fy + sy)
    where
      (fx, fy) = getVec x
      (sx, sy) = getVec y

instance Mon Transform where
  m1 = Trans (Vec (0, 0), 0)
  x >< y = Trans (Vec (x1 + x2, y1 + y2), (a1 + a2) `mod'` 360)
    where
      Trans (Vec (x1, y1), a1) = x
      Trans (Vec (x2, y2), a2) = y

-- | Helper functions that are not directly part of the solution.
pmap :: (a -> b) -> (a, a) -> (b, b)
pmap f (x, y) = (f x, f y)

applyBoth :: (b -> b -> c) -> (a -> b) -> a -> a -> c
applyBoth f g x1 x2 = f (g x1) (g x2)

cos :: Rational -> Rational
sin x' = (4 * x * (180 - x)) / (40500 - x * (180 - x))
  where
    x = x' `mod'` 360

sin :: Rational -> Rational
cos = sin . (90 +)

trR2 :: Transform -> R2 -> R2
trR2 (Trans (Vec (x, y), a)) (px, py) = _doRotate a (x + px, y + py)
  where
    _doRotate a ~(x, y) = (x * c - y * s, x * s + y * c)
      where
        c = cos a
        s = sin a

-- | Functions that are part of the solution
vec :: R2 -> Vec
vec = Vec

point :: R2 -> Point
point = Point

line :: (R, R) -> (R, R) -> Picture
line x y = Picture [(x, y)]

rectangle :: R -> R -> Picture
rectangle w h = Picture [((0, 0), (w, 0)), ((w, 0), (w, h)), ((w, h), (0, h)), ((0, h), (0, 0))]

(&) :: Picture -> Picture -> Picture
x & y = Picture $ getLineList x ++ getLineList y

renderScaled :: Int -> Picture -> IntRendering
renderScaled x p = _render $ _scale x (getLineList p)
  where
    _scale fac = map $ pmap (pmap (* fromIntegral fac))
    _render = map $ pmap $ pmap round

translate :: Vec -> Transform
translate = Trans . (, 0)

rotate :: R -> Transform
rotate = Trans . (m1, ) . (`mod'` 360)

fullCircle :: R
fullCircle = 0

trpoint :: Transform -> Point -> Point
trpoint t = point . trR2 t . getCoords

trvec :: Transform -> Vec -> Vec
trvec t p = Vec $ getCoords $ trpoint t (point $ getVec p)

transform :: Transform -> Picture -> Picture
transform tr p = Picture $ map (pmap $ trR2 tr) (getLineList p)