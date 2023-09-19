import Data.List
import Practica3
  ( Token(Assign, If, Then, Else, Seq, While, Do, Skip, Boolean, Equal, And, Not, Loc, Number, LP, RP, Sum)
  )
import Test.HUnit
import Control.Exception (evaluate, SomeException, try)

test_1 = TestCase (assertEqual "1" 1 1)
test_2 = TestCase (assertEqual "2" 2 2)

tests =
  TestList
    [ test_1
    , test_2
    ]

main = runTestTT tests
