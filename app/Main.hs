module Main (main) where

import Lib (ex1a, ex1b, ex2a, ex2b, ex3a, ex3b, ex4a, ex4b, ex5a, ex5b)

problems :: String -> String -> String
problems "1a" = ex1a
problems "1b" = ex1b
problems "2a" = ex2a
problems "2b" = ex2b
problems "3a" = ex3a
problems "3b" = ex3b
problems "4a" = ex4a
problems "4b" = ex4b
problems "5a" = ex5a
problems "5b" = ex5b
problems _ = const "This is not yet solved"

main :: IO ()
main = do
  putStrLn "What day to solve?"
  ex <- getLine
  executeEx ex

executeEx :: String -> IO ()
executeEx x = do
  input <- readFile ("ex/" ++ x ++ ".txt")
  print (length input)
  putStrLn $ problems x input
