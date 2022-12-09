{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Lib
  ( ex1a,
    ex1b,
    ex2a,
    ex2b,
    ex3a,
    ex3b,
    ex4a,
    ex4b,
    ex5a,
    ex5b
  )
where

import Control.Arrow (Arrow (second, (&&&), first), (***))
import Data.Char (ord, isSpace)
import Data.List (intersect, nub, sort, unfoldr, elemIndex, transpose, uncons)
import GHC.Unicode (isLower)
import Data.Maybe (fromJust, catMaybes)
import Debug.Trace (trace, traceShow)

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p = foldr (\x o -> if p x then [] : o else (x : head o) : tail o) [[]]

replace :: Int -> (a -> a) -> [a] -> [a]
replace i f l = uncurry (++) $ second (uncurry (:) . first f . fromJust . uncons) $ splitAt i l

ex1a :: String -> String
ex1a x = show $ maximum $ map (sum . map (read :: String -> Int)) $ splitOn null $ lines x

ex1b :: String -> String
ex1b x = show $ sum $ take 3 $ reverse $ sort $ map (sum . map (read :: String -> Int)) $ splitOn null $ lines x

ex2a :: String -> String
ex2a input =
  let r x y = (((y - x + 1) `mod` 3) * 3)
      s = succ
   in show $ sum $ map ((\(x, y) -> s y + r x y) . (\[x, _, y] -> (ord x - ord 'A', ord y - ord 'X'))) $ lines input

ex2b :: String -> String
ex2b input =
  let r x y = (((y - x + 1) `mod` 3) * 3)
      s = succ
   in show $ sum $ map ((\(x, y) -> s y + r x y) . (\(x, y) -> (x, (x + y - 1) `mod` 3)) . (\[x, _, y] -> (ord x - ord 'A', ord y - ord 'X'))) $ lines input

ex3a :: String -> String
ex3a input =
  show $ sum $ map (head . map (\x -> (if isLower x then ord x - ord 'a' else ord x - ord 'A' + 26) + 1) . uncurry intersect . (nub *** nub) . (\x -> splitAt (length x `div` 2) x)) $ lines input

ex3b :: String -> String
ex3b input =
  let pairing (x : y : z : l) = Just ((x, (y, z)), l)
      pairing _ = Nothing
   in show $ sum $ map (head . map (\x -> (if isLower x then ord x - ord 'a' else ord x - ord 'A' + 26) + 1) . uncurry intersect . second (uncurry intersect)) $ unfoldr pairing $ lines input

ex4a :: String -> String
ex4a input =
  let parseRange = ((read :: String -> Int) *** (read :: String -> Int)) . second tail . uncurry splitAt . (fromJust . elemIndex '-' &&& id)
      parse = (parseRange *** parseRange) . second tail . uncurry splitAt . (fromJust . elemIndex ',' &&& id)
      calc ((x1, x2), (y1, y2)) | x1 >= y1 && x2 <= y2 = True
      calc ((x1, x2), (y1, y2)) | y1 >= x1 && y2 <= x2 = True
      calc _ = False
  in
    show $ length $ filter id $ map (calc . parse) $ lines input

ex4b :: String -> String
ex4b input =
  let parseRange = ((read :: String -> Int) *** (read :: String -> Int)) . second tail . uncurry splitAt . (fromJust . elemIndex '-' &&& id)
      parse = (parseRange *** parseRange) . second tail . uncurry splitAt . (fromJust . elemIndex ',' &&& id)
      calc ((x1, __), (y1, y2)) | y1 <= x1 && x1 <= y2 = True
      calc ((__, x2), (y1, y2)) | y1 <= x2 && x2 <= y2 = True
      calc ((x1, x2), (y1, __)) | x1 <= y1 && y1 <= x2 = True
      calc ((x1, x2), (__, y2)) | x1 <= y2 && y2 <= x2 = True
      calc _ = False
  in
    show $ length $ filter id $ map (calc . parse) $ lines input

getBox :: [Char] -> Maybe (Maybe Char, [Char])
getBox l | length l >= 3 = Just $ first ((\c -> if isSpace c then Nothing else Just c) . (!! 1)) $ splitAt 4 l
getBox _ = Nothing

parseStack :: [String] -> [[Char]]
parseStack = map catMaybes . transpose . map (unfoldr getBox)

parseMoves :: [Char] -> (Int, Int, Int)
parseMoves = (\[_, x, _, y, _, z] -> (read x, read y - 1, read z - 1) :: (Int, Int, Int)) . splitOn isSpace

simulate :: [[Char]] -> (Int, Int, Int) -> [[Char]]
simulate s (a, f, t) = replace t (reverse (take a (s !! f)) ++) $ replace f (drop a) s

ex5a :: String -> String
ex5a = show . map head . uncurry (foldl simulate) . (parseStack *** map parseMoves) . (init *** tail) . uncurry splitAt . (fromJust . elemIndex [] &&& id) . lines

simulate' :: [[Char]] -> (Int, Int, Int) -> [[Char]]
simulate' s (a, f, t) = replace t (take a (s !! f) ++) $ replace f (drop a) s

ex5b :: String -> String
ex5b = show . map head . uncurry (foldl simulate') . (parseStack *** map parseMoves) . (init *** tail) . uncurry splitAt . (fromJust . elemIndex [] &&& id) . lines