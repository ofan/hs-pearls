{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Supply
  (
    Supply,
    next,
    runSupply,
    showTwo,
  ) where

import Control.Monad.State

newtype Supply s a = S (State [s] a) deriving(Monad)

{-unwrapS :: Supply s a -> State [s] a-}
{-unwrapS (S x) = x-}

{-instance Monad (Supply s) where-}
  {-x >>= f = S(unwrapS x >>= unwrapS . f)-}
  {-return = S . return-}

next ::  Supply s (Maybe s)
next = S $ do st <- get
              case st of
                [] -> return Nothing
                (x:xs) -> do put xs
                             return (Just x)

runSupply ::  Supply t a -> [t] -> (a, [t])
runSupply (S m) = runState m

showTwo :: (Show s) => Supply s String
showTwo = do
  a <- next
  b <- next
  return (show "a: " ++ show a ++ ", b: " ++ show b)

