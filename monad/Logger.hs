module Logger
( Logger,
  Log,
  runLogger,
  record
  ) where

type Log = [String]

globToRegex :: String -> Logger String
runLogger :: Logger a -> (a, Log)
record :: String -> Logger ()

globToRegex cs =
  globToRegex' cs >>= \ds -> return ('^':ds)

globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
globToRegex' ('*':cs) = do
  record "kleene star"
  ds <- globToRegex' cs
  return (".*" ++ ds)
globToRegex' ('[':'!':c:cs) =
  record "character class, negative" >>
  charClass cs >>= \ds ->
  return ("[^" ++ c:ds)
globToRegex' g@('[':c:cs) =
  record "character class" >>
  charClass cs >>= \ds ->
  return g
globToRegex' ('[':_) =
  fail "unterminated character class"

charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` charClass cs

newtype Logger a = Logger { execLogger :: (a, Log) }

runLogger = execLogger
record s = Logger ((), [s])

instance Monad Logger where
  return a = Logger (a, [])
  m >>= k = let (a,w) = execLogger m
               n     = k a
               (b,x) = execLogger n
            in Logger (b, w ++ x)
