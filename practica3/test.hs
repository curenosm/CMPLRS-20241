import Data.List
import Practica3 (Token(Assign, If, Then, Else, Seq, While, Do, Skip, Boolean, Equal, And, Not, Loc, Number, LP, RP, Sum), lexer)
import Main
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
test_lexer_6 = TestCase (assertEqual "lexer \"L2:=1;\""[ Loc 2 , Assign, Number 1, Seq ] (lexer "L2 :=1;"))
test_lexer_7 = TestCase (assertEqual "lexer \"L2:=1; L3:=0;\""[ Loc 2 , Assign, Number 1, Seq, Loc 3, Assign, Number 0, Seq ] (lexer "L2:=1; L3:=0;"))
test_lexer_8 = TestCase (assertEqual "lexer \"L2:=1; L3:=0; while -(L2=L2) do L2:=L2+1; L3:=L3+1\""[ Loc 2 , Assign, Number 1, Seq, Loc 3, Assign, Number 0, Seq, While, Not, LP, Loc 2, Equal, Loc 2, RP, Do, Loc 2, Assign, Loc 2, Sum, Number 1, Seq, Loc 3, Assign, Loc 3, Sum, Number 1] (lexer "L2:=1; L3:=0; while -(L2=L2) do L2:=L2+1; L3:=L3+1"))
test_lexer_9 = TestCase (assertEqual "lexer \"if -(true&false) then skip else skip\""[If, Not, LP, Boolean True, And, Boolean False, RP, Then, Skip, Else, Skip] (lexer "if -(true&false) then skip else skip"))
test_alex_lexer_1 = TestCase (assertEqual "alexScanTokens \"L000000001 := 1\""
  [Loc 1, Assign, Number 1]
  (alexScanTokens "L1 := 1"))

test_alex_lexer_2 = TestCase (assertEqual "alexScanTokens \"L2:=1; L3:=0; while -(L2=L2) do L2:=L2+1; L3:=L3+1\""
  [Loc 2, Assign, Number 1, Seq, Loc 3, Assign, Number 0, Seq, While, Not, LP, Loc 2, Equal, Loc 2, RP, Do, Loc 2, Assign, Loc 2, Sum, Number 1, Seq, Loc 3, Assign, Loc 3, Sum, Number 1]
  (alexScanTokens "L2:=1; L3:=0; while -(L2=L2) do L2:=L2+1; L3:=L3+1"))

test_alex_lexer_3 = TestCase (assertEqual "alexScanTokens \"if -(true&false) then skip else skip\""
  [If, Not, LP, Boolean True, And, Boolean False, RP, Then, Skip, Else, Skip]
  (alexScanTokens "if -(true&false) then skip else skip"))

tests =
  TestList
    [ test_1
    , test_2
    , test_alex_lexer_1
    , test_alex_lexer_2
    , test_alex_lexer_3
    , test_lexer_1
    , test_lexer_2
    , test_lexer_3
    , test_lexer_4
    , test_lexer_5
    , test_lexer_6
    , test_lexer_7
    , test_lexer_8
    , test_lexer_9
    ]

main = runTestTT tests
