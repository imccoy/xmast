module Main where

import Control.Monad (forM_)
import Control.Monad.Free
import Data.List (intersperse)
import System.Console.ANSI

data Tree next = Branch Int next
               | Trunk next
               | Done
  deriving (Functor)

data STree = Pad Int | Peak | LBranch Int | STrunk | RBranch Int


sTree Peak = "/\\"
sTree STrunk = "||"
sTree (LBranch n) = "\\" ++ replicate n '_'
sTree (RBranch n) = replicate n '_' ++ "/"
sTree (Pad n) = replicate n ' '

ioTree :: STree -> IO ()
ioTree t =
  do
    setSGR [SetColor Foreground (brightness t) (color t)]
    putStr (sTree t)
  where
    color STrunk = Green
    color _ = Green
    brightness STrunk = Dull
    brightness _ = Vivid

type TreeM = Free Tree

branch :: Int -> TreeM () 
branch n = liftF (Branch n ())

trunk :: TreeM () 
trunk = liftF (Trunk ())

maxTreeBranch :: TreeM a -> Int
maxTreeBranch (Free (Branch n k)) = max n (maxTreeBranch k) -- pretty good chance this is a space leak
maxTreeBranch (Free (Trunk k)) = maxTreeBranch k
maxTreeBranch _ = 0

runList :: TreeM a -> [[STree]]
runList t = runList' t
  where
    trunkLhsCol = length (sTree $ LBranch $ maxTreeBranch t)
    runList' (Free (Branch 0 k)) = [Pad trunkLhsCol, Peak]:(runList' k)
    runList' (Free (Branch n k)) = [Pad (trunkLhsCol - (n + 1)), LBranch n, STrunk, RBranch n]:(runList' k)
    runList' (Free (Trunk k)) = [Pad trunkLhsCol, STrunk]:(runList' k)
    runList' (Free (Done)) = []
    runList' (Pure _) = []

tree :: TreeM ()
tree = do
  forM_ [0, 2..40] $ \n -> do
    branch n
    trunk
  trunk
  trunk
  trunk

main = do
  let commands = runList tree
  let lines = [map ioTree commandLine | commandLine <- commands]
  forM_ lines $ \line -> do
    sequence line
    putStrLn ""
  setSGR [Reset]
