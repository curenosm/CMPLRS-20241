module Practica1 where

import qualified Data.Set as Set
import Data.List (nub)


fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)



-- Ejercicio 1 y funciones auxiliares
charsSet :: String -> Set.Set Char
charsSet word = Set.fromList word

isAnagram :: String -> String -> Bool
isAnagram s1 s2 = length s1 == length s2 && charsSet s1 == charsSet s2

getAnagrams :: String -> [String] -> [String]
getAnagrams s l = filter (\y -> isAnagram y s) l

groupAnagrams :: [String] -> [[String]]
groupAnagrams l = nub (map (\e -> getAnagrams e l) l)


-- Ejercicio 2
subsets :: [a] -> [[a]]
subsets [x] = [[],[x]]
subsets (x:xs) = (subsets xs ++ map (x:) (subsets xs))


-- Ejercicio 3
majorityTail :: Eq a => [a] -> a -> Int -> a
majorityTail [] m c = m
majorityTail (x:xs) m c
    | x == m = majorityTail xs m (c+1)
    | c == 0 = majorityTail xs x 1
    | otherwise = majorityTail xs m (c-1)

majorityElem :: Eq a => [a] -> a
majorityElem [] = error "Empty list"
majorityElem [x] = x
majorityElem (x:xs) = majorityTail xs x 1

-- Ejercicio 4
coins :: [Int] -> Int -> Bool
coins [] n = n == 0
coins (x:xs) n
  | n > 0 = coins (x:xs) (n - x) || coins xs (n - x) || coins xs n
  | n < 0 = False
  | n == 0 = True

