-- Práctica 1
-- Datos del equipo:
-- 418002485 Cureño Sanchez Misael
-- 318224187 Bernal Núñez Raúl
-- 316641902 García Luna Bobadilla Uriel
-- 419002237 Jardón Cárdenas Juan Diego


module BernalNuñezRaul where

import qualified Data.Set as Set
import Data.List (nub)


-- Ejercicio 1
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
subsets [] = [[]]
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


-- Ejercicio 5
data BST a = Empty | Node a (BST a) (BST a) deriving Show

allLessThan :: Int -> BST Int -> Bool
allLessThan _ Empty = True
allLessThan root (Node v l r) = v < root && allLessThan root l && allLessThan root r

allGreaterThan :: Int -> BST Int -> Bool
allGreaterThan _ Empty = True
allGreaterThan root (Node v l r) = v > root && allGreaterThan root l && allGreaterThan root r

isBST :: BST Int -> Bool
isBST Empty = True
isBST (Node n l r) = allLessThan n l && allGreaterThan n r && isBST l && isBST r


-- Ejercicio 6
elemsInTree :: BST a -> Int
elemsInTree Empty = 0
elemsInTree (Node _ left right) = 1 + elemsInTree right + elemsInTree left

kthElem :: BST a -> Int -> a
kthElem Empty _ = error "No hay tal elemento"
kthElem (Node value left right) n
    | n <= (elemsInTree left) = (kthElem left n)
    | n == ((elemsInTree left) + 1) = value
    | otherwise = (kthElem right (n - elemsInTree left - 1 ))
