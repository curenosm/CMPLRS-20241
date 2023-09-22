import Data.List
import Tokens(alexScanTokens, Token (
  Assign, If, Then, Else,
  Seq, While, Do, Skip,
  Boolean, Equal, And, 
  Not, Loc, Number, LP, RP, Sum))
import Test.HUnit
import Control.Exception (evaluate, SomeException, try)

test_1 = TestCase (assertEqual "1" 1 1)
test_2 = TestCase (assertEqual "2" 2 2)

test_alex_lexer_1 = TestCase (assertEqual "alexScanTokens \"L000000001 := 1\""
  [Loc 1, Assign, Number 1]
  (alexScanTokens "L1 := 1"))


tests =
  TestList
    [ test_1
    , test_2
    , test_alex_lexer_1
    ]

main = runTestTT tests
