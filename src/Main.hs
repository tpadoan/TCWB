module Main where

import Control.Monad
import System.Exit
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BString
import Model
import Logic
import MC

main = do
  putStrLn "Input the file path of the model..."
  filePath <- getLine
  json <- BString.readFile filePath
  let mbnet = extractNetFromJSON json
  when (isNothing mbnet) ((putStrLn "The input model is not correct.") >> exitFailure)
  let Just net = mbnet
  -- putStrLn (show net++"\n")
  -- let (reachables, branch) = states net
  -- putStrLn (show (Set.size reachables)++" reachable states, max branching "++show branch++"\n")

  putStrLn "Input a formula..."
  inputFormula <- getLine
  let mblt = parseFormula inputFormula
  when (isNothing mblt) ((putStrLn "The input formula is not correct.") >> exitFailure)
  let Just (treeF, resTok) = mblt

  putStrLn (show (satisfies net treeF))
