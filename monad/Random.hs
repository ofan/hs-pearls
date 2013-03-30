module MonadRandom where
import System.Random
import Control.Monad.State (State, get, put, modify, runState, state)
import Control.Monad (liftM2, liftM)

rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))

twoBadRandoms :: RandomGen g => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)

twoGoodRandoms :: RandomGen g => g -> ((Int, Int), g)
twoGoodRandoms gen = let (a, gen') = random gen
                         (b, gen'') = random gen'
                     in ((a, b), gen'')

type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
{-getRandom =-}
  {-get >>= \gen ->-}
    {-let (val, gen') = random gen in-}
    {-put gen' >>-}
    {-return val-}
getRandom = state random

getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom

runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do
  oldState <- getStdGen
  let (result, newState) = runState getTwoRandoms oldState
  setStdGen newState
  return result

runOneRandom :: IO Int
runOneRandom = do
  oldGen <- getStdGen
  let (val, gen) = runState getRandom oldGen
  setStdGen gen
  return val

data CountedRandom = CountedRandom {
    crGen :: StdGen,
    crCount :: Int
  }

type CRState = State CountedRandom

getCountedRandom :: Random a => CRState a
getCountedRandom = do
  st <- get
  let (val, gen) = random (crGen st)
  put CountedRandom { crGen = gen, crCount = crCount st + 1}
  return val

getCount :: CRState Int
getCount = crCount `liftM` get

putCount :: Int -> CRState ()
putCount a = modify $ \st ->
    st { crCount = a}

initState :: IO CountedRandom
initState = liftM (flip CountedRandom 0) getStdGen

runCountedRandom :: Int -> IO (Int, Int)
runCountedRandom seed = do
  gen <- getStdGen
  let (val, st) = runState getCountedRandom (CountedRandom gen seed)
  setStdGen $ crGen st
  return (val, crCount st)

