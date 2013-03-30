{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Error

data DivByError a = DivBy0
               | ForbiddenDenominator a
               | OtherDivByError String
               deriving (Eq, Read, Show)

instance Error (DivByError a) where
  strMsg = OtherDivByError

divBy :: Integral a => a -> [a] -> Either (DivByError a) [a]
divBy = divByGeneric

divByGeneric :: (Integral a, MonadError (DivByError a) m) =>
  a -> [a] -> m [a]
divByGeneric _ [] = return []
divByGeneric _ (0:_) = throwError DivBy0
divByGeneric numerator (denom:xs) =
  do next <- divByGeneric numerator xs
     return ((numerator `div` denom) : next)
