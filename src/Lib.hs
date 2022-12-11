{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE LambdaCase #-}

module Lib
  ( problems
  )
where

import Control.Arrow (Arrow (second, (&&&), first), (***))
import Data.Char (ord, isSpace)
import Data.List (intersect, nub, sort, unfoldr, elemIndex, transpose, uncons, findIndex, sortOn, singleton, zip5)
import GHC.Unicode (isLower)
import Data.Maybe (fromJust, catMaybes)

import Util (splitOn, replace)
import Seven (buildNodes, findDirs, calcSize, calcSize', findDirs', name)
import Control.Monad (liftM2)

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
problems "6a" = ex6a
problems "6b" = ex6b
problems "7a" = ex7a
problems "7b" = ex7b
problems "8a" = ex8a
problems _ = const "This is not yet solved"

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

ex6a :: String -> String
ex6a x = show $ (+ 4) $ fromJust $ findIndex ((== 4) . length . nub) $ unfoldr (\l -> if null l then Nothing else Just (take 4 l, tail l)) x

ex6b :: String -> String
ex6b x = show $ (+ 14) $ fromJust $ findIndex ((== 14) . length . nub) $ unfoldr (\l -> if null l then Nothing else Just (take 14 l, tail l)) x

ex7a :: String -> String
ex7a = show . sum . findDirs . calcSize . buildNodes

ex7b :: String -> String
ex7b = show . sortOn snd . filter ((> (30000000 - 70000000 + 43562874)) . snd) . findDirs' . calcSize' . buildNodes

ex8a :: String -> String
ex8a input =
  let
    m = map (map ((read :: String -> Int) . singleton)) $ lines input
    h = length m
    w = length $ head m
    me = map (scanr1 max) m
    mw = map (scanl1 max) m
    mn = transpose $ map (scanl1 max) (transpose m)
    ms = transpose $ map (scanr1 max) (transpose m)
    mc = liftM2 (,) (take h $ iterate succ (0 :: Int)) (take w $ iterate succ (0 :: Int))
    get (y, x) z = if y < 0 || y >= h || x < 0 || x >= w then -1 else z !! y !! x
    m' = map (\(y, x) -> get (y, x) m <= minimum [get (y, x + 1) me, get (y, x - 1) mw, get (y - 1, x) mn, get (y + 1, x) ms]) mc
    m'' = map (\(y, x) -> (get (y, x + 1) me, get (y, x - 1) mw, get (y - 1, x) mn, get (y + 1, x) ms)) mc
    text = unlines $ map show $ unfoldr (\l -> case splitAt w l of
                                            ([], _) -> Nothing
                                            (a, b) -> Just (a, b)
                                        ) m'
  in
    show $ length $ filter not m'

ex8b :: String -> String
ex8b input =
  let
    m = map (map ((read :: String -> Int) . singleton)) $ lines input
    h = length m
    w = length $ head m
  in
    show m
