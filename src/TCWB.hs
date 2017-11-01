module Main where

import qualified Data.Map.Strict as Map
import Commands
import Model
import Logic

main :: IO ()
main = do
  putStrLn "Welcome to the True Concurrency Workbench"
  putStrLn "You can input commands here, type 'help' to see the list of commands or 'help <command>' to get more information about a specific command.\n"

  loop Map.empty where
    loop :: Map.Map [Char] Obj -> IO ()
    loop env = do
      inputCmd <- getLine
      case splitCmd inputCmd of
        Nothing -> do
          putStrLn "Command syntax is not correct\n"
          loop env
        Just [] -> loop env
        Just ("quit":[]) -> return ()
        Just ("exit":[]) -> return ()
        Just ("help":[]) -> do
          cmdList
          loop env
        Just ("help":cmdName:[]) -> do
          help cmdName
          loop env
        Just ("load":filePath:"in":name:[]) -> do
          newEnv <- loadIn filePath name env
          loop newEnv
        Just ("define":name:"=":value) -> do
          newEnv <- defineProp name (mergeCmd value) env
          loop newEnv
        Just ("check":netName:propName:[]) -> do
          check netName propName env
          loop env
        Just ("size":netName:[]) -> do
          netSize netName env
          loop env
        _ -> do
          putStrLn "Unrecognized command\n"
          loop env
