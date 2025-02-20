module Hi where

hello :: IO ()

sayHello :: String ->IO ()
sayHello x = putStrLn ("Thank you for your " ++ x)


hello =
  putStrLn "Input string: " >>
  getLine >>= \ name ->sayHello name

