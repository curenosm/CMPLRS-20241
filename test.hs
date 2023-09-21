import Data.List
import Test.HUnit

test_1 = TestCase (assertEqual "1 == 1" 1 1)

tests = TestList [test_1]

main = runTestTT tests

