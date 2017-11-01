module Commands where

import Control.Exception
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BString
import qualified Data.Map.Strict as Map
import Model
import Logic
import MC

data Obj = MODEL Net | PROPERTY LHP

splitCmd :: [Char] -> Maybe [[Char]]
splitCmd cmd = case splitSep cmd of
  Nothing -> Nothing
  Just l -> Just (filter (not . null) l)

splitSep :: [Char] -> Maybe [[Char]]
splitSep [] = Just [[]]
splitSep (c:s)
  | c == '"' =
    case tilQuotMark s of
      Nothing -> Nothing
      Just (w,r) -> case splitSep r of
        Nothing -> Nothing
        Just l -> Just (w:l)
  | c == ' ' =
    case splitSep s of
      Nothing -> Nothing
      Just l -> Just ([]:l)
  | c == '=' =
    case splitSep s of
      Nothing -> Nothing
      Just l -> Just ([]:"=":l)
  | otherwise =
    case splitSep s of
      Nothing -> Nothing
      Just (w:t) -> Just ((c:w):t)
      Just [] -> Nothing -- branch should be never reached

tilQuotMark :: [Char] -> Maybe ([Char],[Char])
tilQuotMark [] = Nothing
tilQuotMark (c:s)
  | c == '"' = Just ([],s)
  | otherwise =
    case tilQuotMark s of
      Nothing -> Nothing
      Just (w,r) -> Just (c:w,r)

mergeCmd :: [[Char]] -> [Char]
mergeCmd [] = []
mergeCmd (s:l) = s ++ (' ':(mergeCmd l))

cmdList :: IO ()
cmdList = do
  putStrLn $ "A command list ...\n"

help :: [Char] -> IO ()
help cmdName = do
  putStrLn $ "Help with command \n" ++ cmdName

loadIn :: [Char] -> [Char] -> Map.Map [Char] Obj -> IO (Map.Map [Char] Obj)
loadIn filePath name env = do
  result <- try (BString.readFile filePath) :: IO (Either SomeException BString.ByteString)
  case result of
    Left e -> do
      putStrLn "File path is not correct\n"
      return env
    Right json -> case extractNetFromJSON (json) of
      Nothing -> do
        putStrLn "The input model is not correct\n"
        return env
      Just net -> do
        putStrLn (name ++ " = " ++ show net ++ "\n")
        return (Map.insert name (MODEL net) env)

defineProp :: [Char] -> [Char] -> Map.Map [Char] Obj -> IO (Map.Map [Char] Obj)
defineProp name value env = case parseFormula value of
  Nothing -> do
    putStrLn "The input formula is not correct\n"
    return env
  Just prop -> do
    putStrLn (name ++ " = " ++ show prop ++ "\n")
    return (Map.insert name (PROPERTY prop) env)

check :: [Char] -> [Char] -> Map.Map [Char] Obj -> IO ()
check netName propName env = case Map.lookup netName env of
  Nothing -> putStrLn (netName ++ " is undefined\n")
  Just (PROPERTY _) -> putStrLn (netName ++ " is not a model\n")
  Just (MODEL net) -> case Map.lookup propName env of
    Nothing -> putStrLn (propName ++ " is undefined\n")
    Just (MODEL _) -> putStrLn (propName ++ " is not a property\n")
    Just (PROPERTY prop) -> putStrLn (show (satisfies net prop))

netSize :: [Char] -> Map.Map [Char] Obj -> IO ()
netSize name env =
  case Map.lookup name env of
    Nothing -> putStrLn (name ++ " is undefined\n")
    Just (PROPERTY _) -> putStrLn (name ++ " is not a model\n")
    Just (MODEL net) -> do
      let (reachables, branch) = states net
      putStrLn (name ++ " has " ++ show (Set.size reachables) ++ " reachable states, max branching " ++ show branch ++ "\n")
