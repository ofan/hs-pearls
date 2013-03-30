{-# LANGUAGE DeriveDataTypeable #-}


import Data.Dynamic
import Control.Exception

data SqlError = SqlError { seState :: String,
                           seNativeError :: Int,
                           seErrorMsg :: String }
                deriving (Eq, Show, Read, Typeable)

catchSql :: IO a -> (SqlError -> IO a) -> IO a
catchSql = catchDyn

handleSql :: (SqlError -> IO a) -> IO a -> IO a
handleSql = flip catchSql

handleSqlError :: IO a -> IO a
handleSqlError action =
  catchSql action handler
  where handler e = fail ("SQL error: " ++ show e)

throwSqlError :: String -> Int -> String -> a
throwSqlError state nativeerror errormsg =
  throwDyn (SqlError state nativeerror errormsg)

throwSqlErrorIO :: String -> Int -> String -> IO a
throwSqlErrorIO state nativeerror errormsg =
  evaluate (throwSqlError state nativeerror errormsg)

