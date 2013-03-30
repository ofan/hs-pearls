sum lst = sum' lst 0
  where
  sum' (x:xs) a = sum' xs (a+x)
  sum' [] a = a

{-sumM :: (Monad m, Num a, Num b) => m a -> m a -> b-}
sumM a b = do
    x <- a
    y <- b
    x + y

data Person = Person {
  name :: String,
  ssn :: Int,
  tel :: String
  }
  | Nobody
  deriving (Show)

data Student = Student {
  grade :: String
  } deriving (Show)


