import Data.List
import Tokens (lexer)
import Practica3
  ( Token(Assign, If, Then, Else, Seq, While, Do, Skip, Boolean, Equal, And, Not, Loc, Number, LP, RP, Sum)
  )
import Test.HUnit
import Control.Exception (evaluate, SomeException, try)

test_1 = TestCase (assertEqual "1" 1 1)
test_2 = TestCase (assertEqual "2" 2 2)

test_alex_lexer_1 = TestCase (assertEqual "lexer \"x := 1\""
  [Assign, Loc "x", Equal, Number 1]
  (lexer "x := 1"))


tests =
  TestList
    [ test_1
    , test_2
    , test_alex_lexer_1
    ]

main = runTestTT tests
