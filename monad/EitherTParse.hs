{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EitherTParse
  (
    Parse
  {-, evalParse-}
  ) where

import Control.Monad.State
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as L

newtype EitherT a m b = EitherT {
  runEitherT :: m (Either a b)
  }

instance (Monad m) => Monad (EitherT a m) where
  return = EitherT . return . Right
  e >>= f = EitherT $ runEitherT e >>= switch f
    where switch ff (Right x) = runEitherT (ff x)
          switch _  (Left err)= return (Left err)

instance MonadTrans (EitherT l) where
  lift m = EitherT (Right `liftM` m)

instance (MonadState s m) => MonadState s (EitherT l m) where
  get = lift get
  put k = lift (put k)

data ParseState = ParseState {
    string :: L.ByteString
  , offset :: Int64
  } deriving (Show)

newtype Parse a = P {
  runP :: EitherT String (State ParseState) a
  } deriving (Monad, MonadState ParseState)

