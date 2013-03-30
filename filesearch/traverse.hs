module Traverse (traverse, Info(..), isDirectory, getUsefulContents, getInfo) where

import System.Directory (Permissions, getModificationTime,
                        getPermissions, getDirectoryContents, searchable)
import System.IO (hFileSize, withFile, IOMode(..))
import Control.Exception (SomeException, handle)
import Control.Monad (liftM, forM)
import System.FilePath ((</>))
import System.Time (ClockTime)

data Info = Info {
  infoPath :: FilePath,
  infoPerms :: Maybe Permissions,
  infoSize :: Maybe Integer,
  infoModTime :: Maybe ClockTime
  } deriving (Eq, Ord, Show)

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle handler (Just `liftM` act)
              where handler :: SomeException -> IO (Maybe a)
                    handler _ = return Nothing

getInfo :: FilePath -> IO Info
getInfo path = withFile path ReadMode (\h -> do
  perms <- maybeIO $ getPermissions path
  size <- maybeIO $ hFileSize h
  modtime <- maybeIO $ getModificationTime path
  return $ Info path perms size modtime)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info ->
    if isDirectory info && infoPath info /= path
      then traverse order (infoPath info)
      else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms
