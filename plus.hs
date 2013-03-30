import Data.List
import Data.Maybe
import Data.Char (ord, digitToInt, isDigit)
import Data.Bits (shiftL, (.&.), (.|.))

a `plus` b = a + b
data a `Pair` b = a `Pair` b
                  deriving(Show)

myhead xs | length xs > 0 = head xs
          | otherwise = Just 'Z'

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just (tail xs)

square :: [Double] -> [Double]
square (x:xs) = x*x : square xs
square [] = []

base = 65521

adler32 :: String -> Int
adler32 xs = helper 1 0 xs
  where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                b' = (a' + b) `mod` base
                            in helper a' b' xs
        helper a b _ = (b `shiftL` 16) .|. a

adler32_ :: String -> Int
adler32_ xs = let (a,b) = foldl step (1,0) xs
              in (b `shiftL` 16) .|. a
  where step (a,b) x =  let a' = a + (ord x .&. 0xff)
                        in (a' `mod` base, (a' + b) `mod` base)

myfoldl :: (a->b->a) -> a->[b]->a
myfoldl f z xs = (foldr step id xs) z
  where step x g a = g (f a x)

asInt_fold :: String -> Int
asInt_fold ('-':xs) = -(asInt_fold xs)
asInt_fold ('+':xs) = asInt_fold xs
asInt_fold x = foldl' acc 0 x
                 where acc x y = x*10 + (digitToInt y)

type ErrorMessage = String
--asInt_either :: String -> Either ErrorMessage Int

myconcat :: [[a]] -> [a]
myconcat x = foldr (++) [] x

mytakeWhile :: (a->Bool) -> [a] -> [a]
mytakeWhile f x = loop x [] where loop (x:xs) l | f x = x:(loop xs l) | otherwise = l

mytakeWhile_fold :: (a->Bool) -> [a] -> [a]
mytakeWhile_fold f x = foldr (\ x y -> if f x then x:y else []) [] x
