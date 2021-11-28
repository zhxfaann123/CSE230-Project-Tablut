module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn $ "someFunc" ++ (show 12)
