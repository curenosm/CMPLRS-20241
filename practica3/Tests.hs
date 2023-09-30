import Data.List
import Tokens (lexer, Token(..)) -- Tokens.hs
import Test.HUnit
import Control.Exception (evaluate, SomeException, try)

test_1 = TestCase (assertEqual "1" 1 1)

test_2 = TestCase (assertEqual "2" 2 2)

test_alex_lexer_1 = TestCase (assertEqual "alexScanTokens \"L000000001 := 1\""
  [Loc 1, Assign, Number 1]
  (lexer "L1 := 1"))

test_alex_lexer_2 = TestCase (assertEqual "alexScanTokens \"L2:=1; L3:=0; while -(L2=L2) do L2:=L2+1; L3:=L3+1\""
  [Loc 2, Assign, Number 1, Seq, Loc 3, Assign, Number 0, Seq, While, Not, LP, Loc 2, Equal, Loc 2, RP, Do, Loc 2, Assign, Loc 2, Sum, Number 1, Seq, Loc 3, Assign, Loc 3, Sum, Number 1]
  (lexer "L2:=1; L3:=0; while -(L2=L2) do L2:=L2+1; L3:=L3+1"))

test_alex_lexer_3 = TestCase (assertEqual "alexScanTokens \"if -(true&false) then skip else skip\""
  [If, Not, LP, Boolean True, And, Boolean False, RP, Then, Skip, Else, Skip]
  (lexer "if -(true&false) then skip else skip"))

tests =
  TestList
    [ test_1
    , test_2
    , test_alex_lexer_1
    , test_alex_lexer_2
    , test_alex_lexer_3
    ]

main = runTestTT tests
