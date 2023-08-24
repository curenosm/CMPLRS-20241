import Practica1 (fibonacci)
import Test.HUnit

test1 = TestCase (assertEqual "fibonacci 0" 0 (fibonacci 0))
test2 = TestCase (assertEqual "fibonacci 1" 1 (fibonacci 1))
test3 = TestCase (assertEqual "fibonacci 5" 5 (fibonacci 5))

tests = TestList [test1, test2, test3]

main = runTestTT tests
