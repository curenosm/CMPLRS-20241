import Practica1 (fibonacci, groupAnagrams, subsets, majorityElem, coins, isBST, BST(Node, Empty), kthElem)
import Test.HUnit
import Data.List


test_anagrams_1 = TestCase (assertEqual "groupAnagrams [\"eat\", \"tea\", \"tan\", \"ate\", \"nat\", \"bat\"]" [["eat","tea","ate"],["tan","nat"],["bat"]] (groupAnagrams ["eat", "tea", "tan", "ate", "nat", "bat"]))

test_majority_1 = TestCase (assertEqual "majorityElem [3,2,3]" 3 (majorityElem [3,2,3]))
test_majority_2 = TestCase (assertEqual "majorityElem [2,2,1,1,1,2,2]" 2 (majorityElem [2,2,1,1,1,2,2]))
test_majority_3 = TestCase (assertEqual "majorityElem [2,2,2]" 2 (majorityElem [2,2,2]))
test_majority_4 = TestCase (assertEqual "majorityElem [2,2,2,1,1,1]" 2 (majorityElem [2,2,2,1,1,1]))
test_majority_5 = TestCase (assertEqual "majorityElem [1,1,1,2,2,2]" 1 (majorityElem [1,1,1,2,2,2]))

sortNested :: (Ord a) => [[a]] -> [[a]]
sortNested = map sort . sort

test_groupAnagrams_1 = TestCase (assertEqual "groupAnagrams ['eat','tea','tan','ate','nat','bat']" 
  (sortNested [["eat","tea","ate"],["tan","nat"],["bat"]]) 
  (sortNested (groupAnagrams ["eat","tea","tan","ate","nat","bat"])))

test_groupAnagrams_2 = TestCase (assertEqual "groupAnagrams ['hello','','world','wldro','hlloe','a','aa']" 
  (sortNested [["hello","hlloe"],[""],["world","wldro"],["a"],["aa"]]) 
  (sortNested (groupAnagrams ["hello","","world","wldro","hlloe","a","aa"])))

test_subsets_1 = TestCase (assertEqual "subsets [1,2,3]" 
  (sort [[] ,[3] ,[2] ,[2 ,3] ,[1] ,[1 ,3] ,[1 ,2] ,[1 ,2 ,3]]) 
  (sort (subsets [1,2,3])))

test_subsets_2 = TestCase (assertEqual "subsets ['a','b','c','d']" 
  (sort ["","d","c","cd","b","bd","bc","bcd","a","ad","ac","acd","ab","abd","abc","abcd"]) 
  (sort (subsets ['a','b','c','d'])))

test_subsets_3 = TestCase (assertEqual "subsets []" 
  (sort [[]]) 
  (sort (subsets ([] :: [Int]))))

test_subsets_4 = TestCase (assertEqual "subsets [4]" 
  (sort [[] ,[4]]) 
  (sort (subsets [4])))

test_subsets_5 = TestCase (assertEqual "subsets ['x','y']" 
  (sort ["","x","y","xy"]) 
  (sort (subsets ['x','y'])))


test_coins_1 = TestCase (assertEqual "coins [2,5] 8" 
  True 
  (coins [2,5] 8))

test_coins_2 = TestCase (assertEqual "coins [2,4,6] 21" 
  False 
  (coins [2,4,6] 21))

test_coins_3 = TestCase (assertEqual "coins [1,2,3] 6" 
  True 
  (coins [1,2,3] 6))  -- Possible with [3,3] or [1,2,3]

test_coins_4 = TestCase (assertEqual "coins [5,10] 3" 
  False 
  (coins [5,10] 3))  -- Impossible to make 3 with denominations 5 and 10.

test_coins_5 = TestCase (assertEqual "coins [5,10,20] 35" 
  True 
  (coins [5,10,20] 35))  -- Possible with [5,10,20]

test_isBST_1 = TestCase (assertEqual "isBST (Node 3 (Node 1 Empty (Node 2 Empty Empty)) (Node 4 Empty Empty))"
    True
    (isBST (Node 3 (Node 1 Empty (Node 2 Empty Empty)) (Node 4 Empty Empty))))

test_isBST_2 = TestCase (assertEqual "isBST (Node 3 (Node 1 Empty (Node 3 Empty Empty)) (Node 4 Empty Empty))"
    False
    (isBST (Node 3 (Node 1 Empty (Node 3 Empty Empty)) (Node 4 Empty Empty))))

test_isBST_3 = TestCase (assertEqual "isBST Empty"
    True
    (isBST Empty))  -- An empty tree should be considered a valid BST.

test_isBST_4 = TestCase (assertEqual "isBST (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))"
    True
    (isBST (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))))  -- Straight forward valid BST.


test_kthElem_1 = TestCase (assertEqual 
    "kthElem (Node 3 (Node 1 Empty (Node 2 Empty Empty)) (Node 4 Empty Empty)) 2" 
    2 
    (kthElem (Node 3 (Node 1 Empty (Node 2 Empty Empty)) (Node 4 Empty Empty)) 2))

test_kthElem_2 = TestCase (assertEqual 
    "kthElem (Node 5 (Node 3 (Node 2 (Node 1 Empty Empty) Empty) (Node 4 Empty Empty)) (Node 6 Empty Empty)) 4" 
    4 
    (kthElem (Node 5 (Node 3 (Node 2 (Node 1 Empty Empty) Empty) (Node 4 Empty Empty)) (Node 6 Empty Empty)) 4))

test_kthElem_3 = TestCase (assertEqual 
    "kthElem (Node 1 Empty Empty) 1" 
    1 
    (kthElem (Node 1 Empty Empty) 1))  -- A single node tree.

test_kthElem_4 = TestCase (assertEqual 
    "kthElem (Node 2 (Node 1 Empty Empty) (Node 3 Empty (Node 4 Empty Empty))) 4" 
    4 
    (kthElem (Node 2 (Node 1 Empty Empty) (Node 3 Empty (Node 4 Empty Empty))) 4))  -- Last element of a 4 node tree.

tests = TestList [test_anagrams_1, test_majority_1, test_majority_2, test_majority_3, test_majority_4, test_majority_5, test_groupAnagrams_1, test_groupAnagrams_2, test_subsets_1, test_subsets_2, test_subsets_3, test_subsets_4, test_subsets_5, test_coins_1,test_coins_2,test_coins_3,test_coins_4,test_coins_5, test_isBST_1, test_isBST_2, test_isBST_3, test_isBST_4, test_kthElem_1, test_kthElem_2, test_kthElem_3, test_kthElem_4]

main = runTestTT tests
