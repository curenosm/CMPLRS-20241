import qualified Data.Set as Set
import Data.List (nub)

charsSet :: String -> Set.Set Char
charsSet word = Set.fromList word

isAnagram :: String -> String -> Bool
isAnagram s1 s2 = length s1 == length s2 && charsSet s1 == charsSet s2

getAnagrams :: String -> [String] -> [String]
getAnagrams s l = filter (\y -> isAnagram y s) l

groupAnagrams :: [String] -> [[String]]
groupAnagrams l = nub (map (\e -> getAnagrams e l) l)

subsets :: [a] -> [[a]]
subsets [x] = [[],[x]]
subsets (x:xs) = (subsets xs ++ map (x:) (subsets xs))
