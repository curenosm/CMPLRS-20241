import Data.List
import Practica3 (Token(Assign, If, Then, Else, Seq, While, Do, Skip, Boolean, Equal, And, Not, Loc, Number, LP, RP, Sum), lexer)

import Test.HUnit
import Control.Exception (evaluate, SomeException, try)

{- 

lexer :: String -> [Token]
lexer [] = []
lexer (' ':xs) = lexer xs
lexer ('\n':xs) = lexer xs
lexer ('\t':xs) = lexer xs
lexer (';':xs) = Seq : lexer xs
lexer ('&':xs) = And : lexer xs
lexer ('-':xs) = Not : lexer xs
lexer ('=':xs) = Equal : lexer xs
lexer (':':'=':xs) = Assign : lexer xs
lexer ('s':'k':'i':'p':xs) = Skip : lexer xs
lexer ('i':'f':xs) = If : lexer xs
lexer ('t':'h':'e':'n':xs) = Then : lexer xs
lexer ('e':'l':'s':'e':xs) = Else : lexer xs
lexer ('w':'h':'i':'l':'e':xs) = While : lexer xs
lexer ('d':'o':xs) = Do : lexer xs
lexer ('(':xs) = LP : lexer xs
lexer (')':xs) = RP : lexer xs
lexer ('+':xs) = Sum : lexer xs
lexer ('t':'r':'u':'e':xs) = Boolean True : lexer xs
lexer ('f':'a':'l':'s':'e':xs) = Boolean False : lexer xs
lexer (x:xs)
    | isDigit x = Number (read (x : takeWhile isDigit xs)) : lexer (dropWhile isDigit xs)
    | x == 'L' = Loc (read (takeWhile isDigit xs)) : lexer (dropWhile isDigit xs)
    | otherwise = error ("lexer: unexpected character " ++ [x])  
-}

test_lexer_1 = TestCase (assertEqual "lexer \"\"" [] (lexer ""))
test_lexer_2 = TestCase (assertEqual "lexer \" \" " [] (lexer " "))
test_lexer_3 = TestCase (assertEqual "lexer \"\n\"" [] (lexer "\n"))
test_lexer_4 = TestCase (assertEqual "lexer \"\t\"" [] (lexer "\t"))
test_lexer_5 = TestCase (assertEqual "lexer \";\"" [Seq] (lexer ";"))

tests = TestList [ test_lexer_1 ]
main = runTestTT tests
