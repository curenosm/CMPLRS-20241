import Practica1 (fibonacci, groupAnagrams, subsets, majorityElem)
import Test.HUnit

test1 = TestCase (assertEqual "fibonacci 0" 0 (fibonacci 0))
test2 = TestCase (assertEqual "fibonacci 1" 1 (fibonacci 1))
test3 = TestCase (assertEqual "fibonacci 5" 5 (fibonacci 5))

test_anagrams_1 = TestCase (assertEqual "groupAnagrams [\"eat\", \"tea\", \"tan\", \"ate\", \"nat\", \"bat\"]" [["eat","tea","ate"],["tan","nat"],["bat"]] (groupAnagrams ["eat", "tea", "tan", "ate", "nat", "bat"]))
test_subsets_1 = TestCase (assertEqual "subsets [1]" [[],[1]] (subsets [1]))
test_subsets_2 = TestCase (assertEqual "subsets [1,2,3]" [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]] (subsets [1,2,3]))
test_majority_1 = TestCase (assertEqual "majorityElem [3,2,3]" 3 (majorityElem [3,2,3]))
test_majority_2 = TestCase (assertEqual "majorityElem [2,2,1,1,1,2,2]" 2 (majorityElem [2,2,1,1,1,2,2]))

tests = TestList [test1, test2, test3, test_anagrams_1, test_subsets_1, test_subsets_2, test_majority_1, test_majority_2]

main = runTestTT tests
