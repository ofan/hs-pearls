import qualified Data.ByteString.Lazy as L
import System.Environment (getArgs)

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
  where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
  content <- L.readFile path
  return (hasElfMagic content)

main = do
  args <- getArgs
  iself <- isElfFile $ head args
  if iself then
      putStrLn $ (head args) ++ " is an elf file"
  else
      putStrLn $ (head args) ++ " is not an elf file"
