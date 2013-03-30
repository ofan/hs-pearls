{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString.Char8 as B
import Data.Conduit

count :: (MonadResource m) => Conduit ByteString m Int
count = do
  b <- await
  case b of
    Nothing -> yield 0
    Just bs -> yield $ B.length bs

showCount :: (MonadResource m) => Conduit Int m ByteString
showCount = fmap (maybe "0" (B.pack . show)) await >>= yield
