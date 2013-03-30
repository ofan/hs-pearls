{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module SupplyClass
  (
    MonadSupply(..),
    S.runSupply,
    showTwo,
  ) where
import qualified Supply as S

class (Monad m) => MonadSupply s m | m -> s where
  next :: m (Maybe s)

instance MonadSupply s (S.Supply s) where
  next = S.next

showTwo :: (Show s, Monad m, MonadSupply s m) => m String
showTwo =do
  a <- next
  b <- next
  return (show "a: " ++ show a ++ ", b: " ++ show b)
