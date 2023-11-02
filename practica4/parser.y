{
-- Datos del equipo:
-- 418002485 Cureño Sanchez Misael
-- 318224187 Bernal Núñez Raúl
-- 316641902 García Luna Bobadilla Uriel
-- 419002237 Jardón Cárdenas Juan Diego

module Main where

import Data.Char
import Data.List (nub)
}

%name parser
%tokentype { Token }
%error { parseError }

%token
    if           { If }
    then         { Then }
    else         { Else }
    while        { While }
    do           { Do }
    skip         { Skip }
    bool         { Boolean $$ }
    L            { Loc $$ }
    int          { Number $$ }
    '='          { Equal }
    ';'          { Seq }
    '('          { LP }
    ')'          { RP }
    '-'          { Not }
    '+'          { Sum }
    '&'          { And }
    ":="         { Assign }
%%

C : L ":=" E { AssignASA (LocASA $1) $3 }
    | if B then C else C { IfThenElse $2 $4 $6 }
    | '(' C ';' C ')' { SeqASA $2 $4 }
    | while B do C { WhileDo $2 $4 }
    | skip { SkipASA }

B : bool { BoolASA $1 }
  | E '=' E { EqualASA $1 $3 }
  | '&' B B { AndASA $2 $3 }
  | '-' B { NotASA $2 }

E : L { LocASA $1 }
  | int { NumberASA $1 }
  | '(' E '+' E ')' { SumASA $2 $4 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data C = AssignASA E E | IfThenElse B C C | SeqASA C C | WhileDo B C | SkipASA deriving Show

data B = BoolASA Bool | EqualASA E E | AndASA B B | NotASA B deriving Show

data E = LocASA Int | NumberASA Int | SumASA E E deriving Show

data Token = Assign | If | Then | Else | Seq | While | Do | Skip | Boolean Bool | Equal | And | Not | Loc Int | Number Int | LP | RP | Sum deriving Show

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

main = getContents >>= print . parser . lexer

{- Ejemplos -}

-- parser [LP, Loc 2, Assign, Number 1, Seq, LP, Loc 3, Assign, Number 0, Seq, While, Not, Loc 2, Equal, Loc 2, Do, LP, Loc 2, Assign, LP, Loc 2, Sum, Number 1, RP, Seq, Loc 3, Assign, LP, Loc 3, Sum, Number 1, RP, RP, RP, RP]
-- SeqASA (AssignASA (LocASA 2) (NumberASA 1)) (SeqASA (AssignASA (LocASA 3) (NumberASA 0)) (WhileDo (NotASA (EqualASA (LocASA 2) (LocASA 2))) (SeqASA (AssignASA (LocASA 2) (SumASA (LocASA 2) (NumberASA 1))) (AssignASA (LocASA 3) (SumASA (LocASA 3) (NumberASA 1))))))

-- parser $ lexer "(L2:=1; (L3:=0; while -L2 = L2 do (L2:=(L2+1); L3:=(L3+1))))"
-- SeqASA (AssignASA (LocASA 2) (NumberASA 1)) (SeqASA (AssignASA (LocASA 3) (NumberASA 0)) (WhileDo (NotASA (EqualASA (LocASA 2) (LocASA 2))) (SeqASA (AssignASA (LocASA 2) (SumASA (LocASA 2) (NumberASA 1))) (AssignASA (LocASA 3) (SumASA (LocASA 3) (NumberASA 1))))))

}
