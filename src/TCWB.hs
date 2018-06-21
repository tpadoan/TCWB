module Main where

import System.IO
import Control.Exception (catchJust,AsyncException(UserInterrupt))
import qualified Data.Map.Strict as Map
import Commands
import Model
import Logic

reploop :: Map.Map [Char] Obj -> IO ()
reploop env = catchJust
  (\e -> case e of
    UserInterrupt -> Just()
    _ -> Nothing)
  (do
    putStr "> "
    hFlush stdout
    inputCmd <- getLine
    case splitCmd inputCmd of
      Nothing -> do
        putStrLn "Command syntax is not correct\n"
        reploop env
      Just [] -> reploop env
      Just ("quit":[]) -> return ()
      Just ("exit":[]) -> return ()
      Just ("help":[]) -> do
        cmdList
        reploop env
      Just ("help":cmdName:[]) -> do
        help cmdName
        reploop env
      Just ("load":filePath:"in":name:[]) -> do
        newEnv <- loadIn filePath (Just name) env
        reploop newEnv
      Just ("load":filePath:[]) -> do
        newEnv <- loadIn filePath Nothing env
        reploop newEnv
      Just ("define":name:"=":value) -> do
        newEnv <- defineProp name (mergeCmd value) env
        reploop newEnv
      Just ("check":propName:netName:[]) -> do
        check propName netName env
        reploop env
      Just ("size":netName:[]) -> do
        netSize netName env
        reploop env
      Just (name:[]) -> do
        valOf name env
        reploop env
      _ -> do
        putStrLn "Unrecognized command\n"
        reploop env)
  (\_ -> do
    putStrLn "Interrupted by user\n"
    reploop env)

main :: IO ()
main = do
  putStrLn "Welcome to the True Concurrency Workbench"
  putStrLn "You can input commands here, type 'help' to see the list of commands or 'help <command>' to get more information about a specific command.\n"

  reploop Map.empty