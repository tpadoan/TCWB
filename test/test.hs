module Main where

import System.Environment
import Control.Monad
import System.Exit
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BString
import Model
import Logic
import MC

main = do
  args <- getArgs
  let filePath:formula:l = args

  json <- BString.readFile (filePath)
  let mbnet = extractNetFromJSON json
  when (isNothing mbnet) ((putStrLn "The input model is not correct.") >> exitFailure)
  let Just net = mbnet

  let mblt = parseFormula formula
  when (isNothing mblt) ((putStrLn "The input formula is not correct.") >> exitFailure)
  let Just (treeF, resTok) = mblt

  putStrLn (show (satisfies net treeF))
