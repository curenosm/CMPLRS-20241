module Practica1 where

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- Ejercicio 4
coins :: [Int] -> Int -> Bool
coins [] n = n == 0
coins (x:xs) n
  | n > 0 = coins (x:xs) (n - x) || coins xs (n - x) || coins xs n
  | n < 0 = False
  | n == 0 = True
