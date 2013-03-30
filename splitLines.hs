
splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
  let (pre, suf) = break isLineTerminator cs
      in pre:case suf of
               ('\r':'\n':rest) -> splitLines rest
               ('\r':rest)      -> splitLines rest
               ('\n':rest)      -> splitLines rest
               _                -> []

isLineTerminator :: Char -> Bool
isLineTerminator c = c=='\r' || c=='\n'

fixLines :: String -> String
fixLines input = unlines (splitLines input)

fact :: (Num a, Ord a) => a -> a
fact n | n < 2 = 1
       | otherwise = n * fact (n-1)
