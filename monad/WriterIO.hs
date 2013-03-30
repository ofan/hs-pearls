{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WriterIO
  (
    runWriterIO,
    safeHello
  )
  where

import System.IO(IOMode)
import Control.Monad.Writer(Writer, tell, runWriter)
import SafeHello
import Control.Monad.Writer.Class(MonadWriter)
import MonadHandle

data Event = Open FilePath IOMode
           | Put String String
           | Close String
           | GetContents String
            deriving (Show)

newtype WriterIO a = W { runW :: Writer [Event] a }
  deriving (Monad, MonadWriter [Event])

instance MonadHandle [Char] WriterIO where
  openFile path mode = tell [Open path mode] >> return path
  hPutStr h str = tell [Put h str]
  hClose h = tell [Close h]
  hGetContents h = tell [GetContents h] >> return ""

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW
