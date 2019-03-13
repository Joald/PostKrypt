module Main where

import           Data.List          (intersperse)
import           Lib
import           System.Environment (getArgs, getProgName)
import           System.IO
import           Text.Read

-- | Functions used by part B

prologue = "300 400 translate\n"

epilogue = "stroke showpage\n"

helpMessage :: String -> String
helpMessage programName = "Usage: " ++ programName ++ " [scale-factor]"

pointToPostScript :: IntLine -> String
pointToPostScript p =
  let ~(~(x1, y1), ~(x2, y2)) = pmap (pmap show) p
   in unwords [x1, y1, "moveto", x2, y2, "lineto\n"]

showPicture :: IntRendering -> Either String String
showPicture p = Right $ prologue ++ concatMap pointToPostScript p ++ epilogue

-- Functions used by part C

getPicture :: IO Picture
getPicture = return $ rectangle 3 4

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
  let str = getEither $ (flip renderScaled p <$> n) >>= showPicture
  putStrLn str
