import qualified Data.Set as Set
import Data.List (nub)


isAnagram :: String -> String -> Bool
isAnagram s1 s2 = length s1 == length s2 && charsSet s1 == charsSet s2
    where 
        charsSet :: String -> Set.Set Char
        charsSet word = Set.fromList word

getAnagrams :: String -> [String] -> [String]
getAnagrams s l = filter (\y -> isAnagram y s) l

groupAnagrams :: [String] -> [[String]]
groupAnagrams l = nub (map (\e -> getAnagrams e l) l)

groupAnagrams ["hello","", "world", "wldro", "hlloe", "a", "aa"]
groupAnagrams ["eat" ,"tea", "tan", "ate", "nat", "bat"]