import Data.List
import Practica3 (Token(Assign, If, Then, Else, Seq, While, Do, Skip, Boolean, Equal, And, Not, Loc, Number, LP, RP, Sum), lexer)

import Test.HUnit
import Control.Exception (evaluate, SomeException, try)

test_lexer_1 = TestCase (assertEqual "lexer \"\"" [] (lexer ""))

tests = TestList [ test_lexer_1 ]
main = runTestTT tests
