import System.IO.Unsafe
import Data.IORef

test :: IORef [a]
test = unsafePerformIO $ newIORef []

main = do
  writeIORef test [42]
  bang <- readIORef test
  putStrLn (bang :: String)
