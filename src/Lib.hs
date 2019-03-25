{-# LANGUAGE FlexibleInstances    #-}

module Lib
  ( R
  , R2
  , Vec()
  , Point()
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
  -- also export some utils for use in Main
  , pmap
  , trR2
  , Line
  ) where

import           Control.Monad
import           Data.Fixed         (mod')
import           Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import           GHC.IO             (unsafePerformIO)
import           GHC.Real
import           Mon
import           Prelude            hiding (cos, sin)

-- | All the type definitions.
type R = Rational

type R2 = (R, R)

newtype Point = Point
  { getCoords :: R2
  } deriving (Show, Eq)

newtype Vec = Vec
  { getVec :: R2
  } deriving (Show, Eq)

newtype Transform = Transform { getList :: [BaseTransform] } deriving (Show, Eq)

data BaseTransform = Rot R | Trans Vec deriving (Show, Eq)

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

simplifyTransformList :: [BaseTransform] -> [BaseTransform]
simplifyTransformList trl
  | x : xs <- trl, isIdentity x = simplifyTransformList xs
  | Trans v1 : Trans v2 : rest <- trl = simplifyTransformList $ (Trans $ v1 >< v2) : rest
  | Rot a1 : Rot a2 : rest <- trl = simplifyTransformList $ (Rot $ (a1 + a2) `mod'` 360) : rest
  | x1 : xs <- trl = x1 : simplifyTransformList xs
  | otherwise = trl

fix :: Eq a => (a -> a) -> a -> a
fix f x =
  if y == x
    then y
    else fix f y
  where
    y = f x

instance Mon Transform where
  m1 = Transform []
  x >< y = Transform $ fix simplifyTransformList $ getList x ++ getList y

-- | Helper functions that are not directly part of the solution.
isIdentity :: BaseTransform -> Bool
isIdentity t
  | Rot x <- t = x == 0
  | Trans c <- t = c == Vec (0, 0)

pmap :: (a -> b) -> (a, a) -> (b, b)
pmap f (x, y) = (f x, f y)

applyBoth :: (b -> b -> c) -> (a -> b) -> a -> a -> c
applyBoth f g x1 x2 = f (g x1) (g x2)

sin :: Rational -> Rational
sin x' = (4 * x * (180 - x)) / (40500 - x * (180 - x)) * (-signum (x `mod'` 360 - 180))
  where
    x = x' `mod'` 180

cos :: Rational -> Rational
cos = sin . (90 +)

_doRotate :: R -> R2 -> R2
_doRotate a ~(x, y) = (x * c - y * s, x * s + y * c)
  where
    c = cos a
    s = sin a

trBaseR2 :: BaseTransform -> R2 -> R2
trBaseR2 (Trans (Vec (x, y))) (px, py) = (x + px, y + py)
trBaseR2 (Rot a) p                     = _doRotate a p

trR2 :: Transform -> R2 -> R2
trR2 t p = foldl (flip trBaseR2) p $ getList t


toAngle :: BaseTransform -> R
toAngle t
  | Rot a <- t = a
  | otherwise = 0

makeTransform :: BaseTransform -> Transform
makeTransform = Transform . (:[])

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
translate = makeTransform . Trans

rotate :: R -> Transform
rotate = makeTransform . Rot . (`mod'` 360)

fullCircle :: R
fullCircle = 0

trpoint :: Transform -> Point -> Point
trpoint t = point . trR2 t . getCoords

trvec :: Transform -> Vec -> Vec
trvec t v = Vec $ trBaseR2 (Rot $ foldr ((+) . toAngle) 0 $ getList t) $ getVec v

transform :: Transform -> Picture -> Picture
transform tr p = Picture $ map (pmap $ trR2 tr) (getLineList p)
