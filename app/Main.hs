module Main (main) where

import Lib (problems)

main :: IO ()
main = do
  putStrLn "What day to solve?"
  ex <- getLine
  executeEx ex

executeEx :: String -> IO ()
executeEx x = do
  input <- readFile ("ex/" ++ x ++ ".txt")
  print (length input)
  putStrLn $ problems (take 2 x) input
