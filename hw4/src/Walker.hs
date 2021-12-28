{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts      #-}

module Walker where 

import Control.Lens       
import Control.Monad.State 
import Data.Char          
import Data.Text (Text, dropWhile, takeWhile, unpack)
import Data.Text.IO (getLine, putStrLn)
import Prelude hiding (dropWhile, getLine, putStrLn, takeWhile)    
import FileSystemLenses

data WalkerCont = WalkerCont {_fs :: [FS], _root ::FilePath, _info :: [(Int, Int)] }
makeLenses ''WalkerCont

isFile :: FS -> Bool
isFile (File _) = True
isFile _ = False

isDir :: FS -> Bool
isDir = not . isFile

instance Show WalkerCont where
  show (WalkerCont dirs rootPath ((a,b):_)) =
    "You in " ++ getPath dirs ++ "\n" ++
    "Files from root " ++ rootPath ++ " : " ++ show a ++ "\n" ++
    "Dirs from root " ++ rootPath ++ " : " ++ show b ++ "\n"
  show _ = "Error"

getPath :: [FS] -> FilePath
getPath [] = ""
getPath (Dir dirName _ : xs) = getPath xs ++ "\\" ++  dirName
getPath _ = undefined

data Cmd = Up | Cd Text | Unknown

parseCmd :: Text -> Cmd
parseCmd input =
  let cmd = takeWhile (not . isSpace) input
      args = dropWhile isSpace $ dropWhile (not . isSpace) input in
  if args == "" then
    case cmd of
      "up" -> Up
      _  -> Unknown
  else case cmd of
      "cd" -> Cd args
      _ -> Unknown

helper :: (MonadState WalkerCont m, MonadIO m) => m ()
helper = do
  context <- get
  liftIO $ print context
  let (filesCnt, dirsCnt) = head $ _info context
  input <- liftIO getLine
  let cmd = parseCmd input
  case cmd of
    (Cd dir) -> do
      let nxt = head (_fs context) ^? cd (unpack dir)
      case nxt of
        Just _nextDir -> do
          modify ((%~) fs (_nextDir :))
          modify ((%~) info ((filesCnt + length (_nextDir ^.. contents . traversed . filtered isFile), 
                dirsCnt + length (_nextDir ^.. contents . traversed . filtered isDir)) :))
        Nothing -> liftIO $ putStrLn "This directory doesn't exist"
    Up -> if length (_info context) == 1 then
        liftIO $ putStrLn "Root"
      else do
        modify ((%~) fs (drop 1))
        modify ((%~) info (drop 1))
    _ -> liftIO $ putStrLn "Unknown command"
  helper

walker :: FilePath -> IO()
walker path = do
  dir <- getDir path
  let visitorContext = WalkerCont [dir] path [(length (dir ^.. contents . traversed . filtered isFile), 
        length (dir ^.. contents . traversed . filtered isDir))]
  _ <- runStateT helper visitorContext
  return ()