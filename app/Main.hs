module Main where

import           Control.Monad
import           Data.List
import           Data.Maybe         (isNothing)
import           GHC.IO             (unsafePerformIO)
import           System.Environment (getArgs, getProgName)
import           System.IO
import           Text.Read

import           Lib
import           Mon

-- | Functions used by part B
prologue = "300 400 translate\n"

epilogue = "stroke showpage\n"

helpMessage :: String -> String
helpMessage programName = "Usage: " ++ programName ++ " [scale-factor]"

pointToPostScript :: IntLine -> String
pointToPostScript p =
  let ~(~(x1, y1), ~(x2, y2)) = pmap (pmap show) p
   in unwords [x1, y1, "moveto", x2, y2, "lineto\n"]

showPicture :: IntRendering -> String
showPicture = concatMap pointToPostScript

addProEpi :: String -> String
addProEpi x = prologue ++ x ++ epilogue

-- Functions used by part C
errorMessage = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show\n"

data PictureState = PictureState
  { stack     :: [R]
  , pic       :: [Line]
  , pathStart :: Maybe R2
  , trans     :: Transform
  , curPoint  :: Maybe R2
  }

startState :: PictureState
startState = PictureState [] [] Nothing m1 Nothing

parsePicture :: [String] -> Maybe Picture
parsePicture l = Picture . reverse . pic <$> foldl (\s el -> s >>= processLexeme el) (Just startState) l

processLexeme :: String -> PictureState -> Maybe PictureState
processLexeme l state@(PictureState stack pic pathStart transforms currentPoint)
  | l == "moveto", y:x:xs <- stack = Just $ state {
    stack = xs,
    pathStart = Just $ trR2 transforms (x, y),
    curPoint = Just $ trR2 transforms (x, y)
  }
  | l == "lineto", y:x:xs <- stack, Just cPoint <- currentPoint = Just $ state {
    stack = xs,
    pic = (cPoint, trR2 transforms (x, y)) : pic,
    curPoint = Just $ trR2 transforms (x, y)
  }
  | l == "closepath", Just start <- pathStart, Just cPoint <- currentPoint, start /= cPoint = Just $ state {
    pic = pmap (trR2 transforms) (cPoint, start) : pic,
    curPoint = Just start
  }
  | l == "closepath" = Just state
  | l == "translate", y:x:xs <- stack = Just $ state {
    stack = xs,
    trans = translate (vec (x, y)) >< transforms
  }
  | l == "rotate", x:xs <- stack = Just $ state {
    stack = xs,
    trans = rotate x >< transforms
  }
  | l == "add", x1:x2:xs <- stack = Just $ state { stack = x2 + x1 : xs }
  | l == "sub", x1:x2:xs <- stack = Just $ state { stack = x2 - x1 : xs }
  | l == "mul", x1:x2:xs <- stack = Just $ state { stack = x2 * x1 : xs }
  | l == "div", x1:x2:xs <- stack, x1 /= 0 = Just $ state { stack = x2 / x1 : xs }
  | Just x <- readMaybe l :: Maybe Int = Just $ state { stack = fromIntegral x : stack }
  | otherwise = Nothing

    
getPicture :: IO (Maybe Picture)
getPicture = parsePicture . words <$> getContents

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither s Nothing  = Left s
maybeToEither _ (Just x) = Right x

getEither :: Either a a -> a
getEither (Left x)  = x
getEither (Right x) = x

-- The main function.
main :: IO ()
main = do
  programName <- getProgName
  args <- getArgs
  let n | null args = Right 1
        | length args == 1 = readEither $ head args
        | otherwise = Left $ helpMessage programName
  p <- getPicture
  putStrLn $ addProEpi $ getEither $ fmap showPicture $ renderScaled <$> n <*> maybeToEither errorMessage p
