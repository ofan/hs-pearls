import Control.Monad
import Control.Proxy
import System.IO
import qualified Data.Map as M

lines' :: (Proxy p) => Handle -> Producer p String IO ()
lines' h = runIdentityP $ forever $ lift (hGetLine h) >>= respond

promptInt :: (Proxy p) => () -> Producer p Int IO r
promptInt () = runIdentityP $ forever $ do
  lift $ putStrLn "Enter an Integer:"
  n <- lift readLn
  respond n

plusOne :: (Proxy p, Show a, Num a) => () -> Pipe p a a IO ()
plusOne () = runIdentityP $ forever $ do
  a <- request ()
  respond $ a+1

plusTwo :: (Proxy p, Show a, Num a) => () -> Pipe p a a IO ()
plusTwo () = runIdentityP $ forever $ do
  a <- request ()
  respond $ a+2

printer :: (Proxy p, Show a) => () -> Consumer p a IO r
printer () = runIdentityP $ forever $ do
  a <- request ()
  lift $ putStrLn "Received a value:"
  lift $ print a

take' :: (Proxy p) => Int -> () -> Pipe p a a IO ()
take' n () = runIdentityP $ do
  replicateM_ n $ do
    a <- request ()
    respond a
  lift $ putStrLn "You shall not pass!"

threeReqs :: (Proxy p) => () -> Client p Int Bool IO ()
threeReqs () = runIdentityP $ forM_ [1, 3, 1] $ \arg -> do
  lift $ putStrLn $ "Client sends: " ++ show (arg :: Int)
  result <- request arg
  lift $ putStrLn $ "Client receives: " ++ show (result :: Bool)
  lift $ putStrLn "*"

comparer :: (Proxy p) => Int -> Server p Int Bool IO r
comparer = runIdentityK $ foreverK $ \arg -> do
  lift $ putStrLn $ "Server receives: " ++ show arg
  let result = arg > 2
  lift $ putStrLn $ "Server sends: " ++ show result
  respond result

cache :: (Proxy p, Ord key) => key -> p key val key val IO r
cache = runIdentityK $ loop M.empty where
  loop m k = case M.lookup k m of
      Nothing -> do
        val <- request k
        k2  <- respond val
        loop (M.insert k val m) k2
      Just val -> do
        lift $ putStrLn "Used cache!"
        k2 <- respond val
        loop m k2

fork :: (Monad m, Proxy p1, Proxy p2, Proxy p3) =>
  () -> Consumer p1 a (Producer p2 a (Producer p3 a m)) r
fork () =
  runIdentityP . hoist (runIdentityP . hoist runIdentityP) $ forever $ do
    a <- request ()
    lift $ respond a
    lift $ lift $ respond a
