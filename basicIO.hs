main = do
       putStrLn "Greetings! What's your name?"
       getLine
        >>= (\inpStr -> putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!")
