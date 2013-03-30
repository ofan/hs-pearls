{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BloomCheck where

import BloomFilter.Hash (Hashable)
import Data.Word (Word8, Word32)
import System.Random (Random(..), RandomGen)
import Test.QuickCheck
import qualified BloomFilter.Easy as B
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

handyCheck :: Testable a => Int -> a -> IO ()
handyCheck limit = quickCheckWith $ stdArgs { maxSuccess = limit }

falsePositive :: Gen Double
falsePositive = choose (epsilon, 1 - epsilon)
  where epsilon = 1e-6

(=~>) :: Either a b -> (b -> Bool) -> Bool
k =~> f = either (const True) f k

prop_one_present ::  Hashable a => t -> a -> Property
prop_one_present _ elt =
  forAll falsePositive $ \errRate ->
    B.easyList errRate [elt] =~> \filt ->
      elt `B.elem` filt

prop_all_present ::  Hashable a => t -> [a] -> Property
prop_all_present _ xs =
  forAll falsePositive $ \errRate ->
    B.easyList errRate xs =~> \filt ->
      all (`B.elem` filt) xs
