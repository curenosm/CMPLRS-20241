import Practica2(lexer, Token(Number,Sum,Var,Equal,And), ASA(Op, VarASA, NumberASA, BooleanASA), Stack, scanner)
import Test.HUnit
import Data.List

test_1 = TestCase (assertEqual "1" 1 1)

test_lexer_1 = TestCase (assertEqual "lexer '22 3 + var == t &&'"
    (lexer "22 3 + var == t &&")
    [Number 22, Number 3, Sum, Var "var", Equal, Var "t", And])

test_lexer_2 = TestCase (assertEqual "lexer '22 + var var f t == &&'"
    (lexer "22 + var var f t == &&")
    [Number 22, Sum, Var "var", Var "var", Var "f", Var "t", Equal, And])

test_scanner_1 = TestCase (assertEqual "scanner '22 3 + var == t &&'"
    (scanner (lexer "22 3 + var == t &&"))
    (Op And (VarASA "t") (Op Equal (VarASA "var") (Op Sum (NumberASA 3) (NumberASA 22)))))

test_scanner_2 = TestCase (assertEqual "scanner '2 2 + 2 =='"
    (scanner (lexer "2 2 + 2 =="))
    (Op Equal (NumberASA 2) (Op Sum (NumberASA 2) (NumberASA 2))))

tests = TestList [test_1, test_lexer_1, test_lexer_2, test_scanner_1, test_scanner_2]

main = runTestTT tests
