module Sorting where

import Control.Parallel (par, pseq)
import Data.List (sort)

force :: [a] -> ()
force xs = go xs `pseq` ()
          where go (_:ss) = go ss
                go [] = 1

parSort :: (Ord a) => Int -> [a] -> [a]
parSort d list@(x:xs)
  | d <= 0 = sort list
  | otherwise = force greater `par` (force lesser `pseq`
                                     (lesser ++ x:greater))
    where lesser = parSort d' [y | y <- xs, y < x]
          greater= parSort d' [y | y <- xs, y>= x]
          d' = d - 1
parSort _ _ = []
