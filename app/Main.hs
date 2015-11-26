module Main where

import Control.Monad (forM_)
import Control.Monad.Free
import Data.List (intersperse)

data Tree next = Branch Int next
               | Trunk next
               | Done
  deriving (Functor)

data STree = Peak | LBranch Int | STrunk | RBranch Int


sTree Peak = "/\\"
sTree STrunk = "||"
sTree (LBranch n) = "\\" ++ replicate n '_'
sTree (RBranch n) = replicate n '_' ++ "/"

type TreeM = Free Tree

branch :: Int -> TreeM () 
branch n = liftF (Branch n ())

trunk :: TreeM () 
trunk = liftF (Trunk ())

runList :: TreeM a -> [[STree]]
runList (Free (Branch 0 k)) = [Peak]:(runList k)
runList (Free (Branch n k)) = [LBranch n, STrunk, RBranch n]:(runList k)
runList (Free (Trunk k)) = [STrunk]:(runList k)
runList (Free (Done)) = []
runList (Pure _) = []

centerlines :: [String] -> [String]
centerlines lines = map pad lines
  where maxlength = foldl (flip $ max . length) 0 lines
        padSize line = (maxlength - (length line)) `div` 2
        pad line = replicate (padSize line) ' ' ++ line

tree :: TreeM ()
tree = do
  forM_ [0, 2..40] $ \n -> do
    trunk
    branch n
  trunk
  trunk
  trunk

main = do
  let commands = runList tree
  let lines = [concat (map sTree commandLine) | commandLine <- commands]
  mapM putStrLn $ centerlines lines
