{-# LANGUAGE BangPatterns #-}
fib :: Int -> Integer
fib n | n < 2 =1
      | otherwise = fib (n-1) + fib (n-2)

fib2 n = go n (0, 1)
  where
    go !n (!a, !b) | n == 0 = a
                   | otherwise = go (n-1) (b, a+b)

main ::  IO ()
main = print $ fib 40
