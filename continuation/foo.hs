{-# LANGUAGE PackageImports #-}
import "mtl" Control.Monad.Cont
import System.IO
import Control.Exception as E
import Control.Monad.Fix

whatsyourname :: String -> String
whatsyourname name =
  (`runCont` id) $ do
    response <- callCC $ \exit -> do
      validateName name exit
      return $ "Welcome, " ++ name ++ "!"
    return response

validateName name exit = when (null name) (exit "You forgot to tell me your name!")

inputName :: ContT r IO String
inputName = callCC $ \f -> do
  liftIO $ putStrLn "Input your name:"
  name <- liftIO getLine
  f name

inputAge :: ContT r IO Int
inputAge = callCC $ \f -> do
  k <- callCC $ return . fix
  liftIO $ putStrLn "Input your age:"
  age <- liftIO (E.try readLn :: IO (Either SomeException Int))
  case age of
    Left _ -> liftIO (putStrLn "Invalid age.") >> k
    Right age' -> f age'

sayHello :: ContT r IO ()
sayHello = do
  name <- inputName
  age <- inputAge
  liftIO $ putStrLn $ "Your name: " ++ name
  liftIO $ putStrLn $ "Your age: " ++ show age

type Fraction = (Integer, Integer)

ratfloor :: Fraction -> Integer
ratfloor (a,b) = a `div` b
