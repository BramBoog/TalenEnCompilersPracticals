module Refresh where

import Data.Char (digitToInt)
import Debug.Trace (trace)

-- Exercise 3

digitsToNum :: [Int] -> Int
digitsToNum xs = fst $ foldr f (0, 0) xs
  where
    f x (acc, dec) = (acc + x * 10 ^ dec, dec + 1)

stringToInt :: String -> Int
stringToInt = digitsToNum . map digitToInt


data Tree a = Bin (Tree a) (Tree a)
            | Tip a deriving Show

-- Exercise 4

information :: Tree a -> [a]
information (Tip a) = [a]
information (Bin l r) = information l ++ information r

-- Exercise 5

pack :: Tree Int -> String
pack (Tip a) = show a
pack (Bin l r) = "{" ++ pack l ++ "," ++ pack r ++ "}"

-- Exercise 6

unpack :: String -> Tree Int
unpack s = fst $ unpack' s
  where
    unpack' :: String -> (Tree Int, String)
    unpack' ('{':cs) = let (l, ',':cs') = unpack' cs
                           (r, '}':cs'') = unpack' cs'
                        in (Bin l r, cs'')
    unpack' cs = let (xs, cs') = span (\c -> c /= ',' && c /= '}') cs
                     in (Tip(stringToInt xs), cs')
