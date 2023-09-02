import Practica2(lexer, Token(Number,Sum,Var,Equal,And))
import Test.HUnit
import Data.List

test_1 = TestCase (assertEqual "1" 1 1)

test_lexer_1 = TestCase (assertEqual "lexer '22 3 + var == t &&'" 
    (lexer "22 3 + var == t &&")
    [Number 22, Number 3, Sum, Var "var", Equal, Var "t", And])

test_lexer_2 = TestCase (assertEqual "lexer '22 + var var f t == &&'" 
    (lexer "22 + var var f t == &&")
    [Number 22, Sum, Var "var", Var "var", Var "f", Var "t", Equal, And])

tests = TestList [test_1, test_lexer_1, test_lexer_2]

main = runTestTT tests
