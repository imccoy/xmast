module Main where

import Data.List (intersperse)

data Tier = Branch Int
          | Trunk
          | Peak

trunk = "||"

branch 0 = "/\\"
branch n = "\\" ++ replicate n '_' ++ trunk ++ replicate n '_' ++ "/"

centerlines :: [String] -> [String]
centerlines lines = map pad lines
  where maxlength = foldl (flip $ max . length) 0 lines
        padSize line = (maxlength - (length line)) `div` 2
        pad line = replicate (padSize line) ' ' ++ line

main = do
  mapM putStrLn $ centerlines $ (intersperse trunk [branch w | w <- [0,2..20]]) ++ replicate 3 trunk
