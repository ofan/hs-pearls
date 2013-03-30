module BloomFilter.Easy
(
  easyList,
  suggestSizing,
  B.fromList,
  B.elem,
  B.notElem,
  B.length
) where

import qualified BloomFilter as B
import BloomFilter.Hash (doubleHash, Hashable)
import Data.Maybe (mapMaybe)
import Data.Word (Word32)
import Data.List (genericLength)

easyList :: (Hashable a)
         => Double          -- false positive rate (between 0 and 1)
         -> [a]            -- values to populate the filter with
         -> Either String (B.Bloom a)
easyList errRate xs = let size = suggestSizing (genericLength xs) errRate
                          buildBloom (bits, hashes) =
                            Right $ B.fromList (doubleHash hashes) bits xs
                      in either (Left . id) buildBloom size

suggestSizing
  :: Integer               -- expected maximum capacity
  -> Double                -- desired false positive rate
  -> Either String (Word32, Int) -- (filter size, number of hashes)
suggestSizing capacity errRate
  | capacity <= 0               = Left "capacity too small"
  | errRate <= 0 || errRate >= 1 = Left "invalid error rate"
  | null saneSizes             = Left "capacity too large"
  | otherwise                  = Right (minimum saneSizes)
  where saneSizes = mapMaybe sanitize $ sizings capacity errRate
        sanitize (bits, hashes)
          | bits > maxWord32 - 1 = Nothing
          | otherwise            = Just (ceiling bits, truncate hashes)
          where maxWord32 = fromIntegral (maxBound :: Word32)

sizings :: Integer -> Double -> [(Double, Double)]
sizings capacity errRate =
  [((-k) * cap / log (1 - (errRate ** (1 / k))), k) | k <- [1..50]]
  where cap = fromIntegral capacity
