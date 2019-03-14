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

passToAll :: (a -> b -> c -> d -> e -> f) -> f -> (f -> a) -> (f -> b) -> (f -> c) -> (f -> d) -> (f -> e) -> f
passToAll f st a b c d e = f (a st) (b st) (c st) (d st) (e st)

data PictureState = PictureState
  { stack     :: [R]
  , pic       :: [Line]
  , pathStart :: Maybe R2
  , trans     :: Transform
  , curPoint  :: Maybe R2
  }

updateStack :: [R] -> PictureState -> PictureState
updateStack newStack state = passToAll PictureState state (const newStack) pic pathStart trans curPoint

updatePic :: [Line] -> PictureState -> PictureState
updatePic newPic state = passToAll PictureState state stack (const newPic) pathStart trans curPoint

updatePathStart :: Maybe R2 -> PictureState -> PictureState
updatePathStart newPathStart state = passToAll PictureState state stack pic (const newPathStart) trans curPoint

updateTrans :: Transform -> PictureState -> PictureState
updateTrans newTransform state = passToAll PictureState state stack pic pathStart (const newTransform) curPoint

updateCurPoint :: Maybe R2 -> PictureState -> PictureState
updateCurPoint newCurPoint state = passToAll PictureState state stack pic pathStart trans (const newCurPoint)

startState :: PictureState
startState = PictureState [] [] Nothing m1 Nothing

parsePicture :: [String] -> Maybe Picture
parsePicture l = Picture . reverse . pic <$> foldl (\s el -> s >>= processLexeme el) (Just startState) l

processLexeme :: String -> PictureState -> Maybe PictureState
processLexeme l state
  | l == "moveto", y:x:xs <- stack = Just
    $ updateStack xs
    $ updatePathStart (Just (x, y))
    $ updateCurPoint (Just (x, y)) state
  | l == "lineto", y:x:xs <- stack, Just cPoint <- currentPoint = Just
    $ updateStack xs
    $ updatePic (pmap (trR2 transforms) (cPoint, (x, y)) : pic)
    $ updateCurPoint (Just (x, y)) state
  | l == "closepath", Just start <- pathStart, Just cPoint <- currentPoint = Just
    $ updatePic (pmap (trR2 transforms) (cPoint, start) : pic)
    $ updateCurPoint (Just start) state
  | l == "closepath" = Just state
  | l == "translate", y:x:xs <- stack = Just
    $ updateStack xs
    $ updateTrans (translate (vec (x, y)) >< transforms) state
  | l == "rotate", x:xs <- stack = Just
    $ updateStack xs
    $ updateTrans (rotate x >< transforms) state
  | l == "add", x1:x2:xs <- stack = Just $ updateStack (x1 + x2 : xs) state
  | l == "sub", x1:x2:xs <- stack = Just $ updateStack (x2 - x1 : xs) state
  | l == "mul", x1:x2:xs <- stack = Just $ updateStack (x2 * x1 : xs) state
  | l == "div", x1:x2:xs <- stack, x1 /= 0 = Just $ updateStack (x2 / x1 : xs) state
  | Just x <- readMaybe l :: Maybe Int = Just $ updateStack (fromIntegral x : stack) state
  | otherwise = Nothing
  where
    PictureState stack pic pathStart transforms currentPoint = state

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
  let n
        | null args = Right 1
        | length args == 1 = readEither $ head args
        | otherwise = Left $ helpMessage programName
  p <- getPicture
  putStrLn $ addProEpi $ getEither $ fmap showPicture $ renderScaled <$> n <*> maybeToEither errorMessage p
