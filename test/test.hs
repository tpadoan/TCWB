module Main where

import System.Environment
import System.IO
import Control.Exception (catchJust,AsyncException(UserInterrupt))
import qualified Data.Map.Strict as Map
import Commands
import Model
import Logic

main = do
  args <- getArgs
  let filePath:formula:l = args

  env1 <- loadIn filePath (Just "Sys") Map.empty
  env2 <- defineProp "Prop" formula env1

  check "Prop" "Sys" env2
