{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib where

import           Control.Monad
import           Data.Fixed    (mod')
import           GHC.Real
import           Mon
import           Prelude       hiding (cos, sin)

-- | All the type definitions.
type R = Rational

type R2 = (R, R)

type Point = R2

newtype Vec =
  Vec R2
  deriving (Show, Eq)

newtype Transform =
  Trans (Vec, R)
  deriving (Show, Eq)

type Picture = [(Point, Point)]

type IntLine = ((Int, Int), (Int, Int))

type IntRendering = [IntLine]

-- | All the instances.
instance Num Vec where
  Vec (x1, y1) + Vec (x2, y2) = Vec (x1 + x2, y1 + y2)
  (-) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined

instance Mon Vec where
  m1 = Vec (0, 0)
  x >< y = x + y

instance Mon Transform where
  m1 = Trans (Vec (0, 0), 0)
  Trans (Vec (x1, y1), a1) >< Trans (Vec (x2, y2), a2) = Trans (Vec (x1 + x2, y1 + y2), (a1 + a2) `mod'` 360)

-- | Helper functions that are not directly part of the solution.
pmap :: (a -> b) -> (a, a) -> (b, b)
pmap f (x, y) = (f x, f y)

lift :: (b -> b -> c) -> (a -> b) -> a -> a -> c
lift f g x1 x2 = f (g x1) (g x2)

getEither :: Either a a -> a
getEither (Left x)  = x
getEither (Right x) = x

-- | Functions that are part of the solution
vec :: R2 -> Vec
vec = Vec

point :: R2 -> Point
point = id

-- odcinek pomiędzy punktami o podanych współrzędnych
line :: (R, R) -> (R, R) -> Picture
line x y = [(point x, point y)]

-- prostokąt o podanej szerokości i wysokości zaczepiony w (0,0)
rectangle :: R -> R -> Picture
rectangle w h =
  [ (point (0, 0), point (w, 0))
  , (point (w, 0), point (w, h))
  , (point (w, h), point (0, h))
  , (point (0, h), point (0, 0))
  ]

(&) :: Picture -> Picture -> Picture
(&) = (++)

-- Obrazowanie przy danym współczynniku powiększenia
-- z zaokrągleniem do najbliższych wartości całkowitych
renderScaled :: Int -> Picture -> IntRendering
renderScaled x p = _render $ _scale x p
  where
    _scale fac p = p
    _render = map $ pmap $ pmap round

-- przesunięcie o wektor
translate :: Vec -> Transform
translate = Trans . (, 0)

-- obrót wokół punktu (0,0) przeciwnie do ruchu wskazówek zegara
-- jednostki mozna sobie wybrać
rotate :: R -> Transform
rotate = Trans . (m1, ) . (`mod'` 360)

fullCircle :: R -- wartość odpowiadająca 1 pełnemu obrotowi (360 stopni)
fullCircle = 0

cos :: Rational -> Rational
sin x' = (4 * x * (180 - x)) / (40500 - x * (180 - x))
  where
    x = x' `mod'` 360

sin :: Rational -> Rational
cos = sin . (90 +)

trpoint :: Transform -> Point -> Point
trpoint (Trans (Vec (x, y), a)) (px, py) = _doRotate a (x + px, y + py)
  where
    _doRotate a ~(x, y) = (x * c - y * s, x * s + y * c)
      where
        c = cos a
        s = sin a

trvec :: Transform -> Vec -> Vec
trvec t (Vec p) = Vec $ trpoint t p

transform :: Transform -> Picture -> Picture
transform tr = map $ pmap $ trpoint tr
