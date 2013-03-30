import Control.Concurrent
import Control.Concurrent.Async

printHello :: IO ()
printHello = do
  mutex <- newMVar 1 :: IO (MVar Int)
  forkIO $ hello mutex
  forkIO $ hello mutex
  return ()
  where hello m = (>> return ()) $ withMVar m $ \_ -> do
          putStrLn "Hello,world!"
          return 1 :: IO Int

printHello2 ::  IO (Async ())
printHello2 = do
  async $ putStrLn "Hello,"
  async $ putStrLn "world!"

main :: IO ()
main = do
  printHello
  printHello2
  threadDelay 3000
