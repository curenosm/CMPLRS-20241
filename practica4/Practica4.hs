-- Datos del equipo:
-- 418002485 Cureño Sanchez Misael
-- 318224187 Bernal Núñez Raúl
-- 316641902 García Luna Bobadilla Uriel
-- 419002237 Jardón Cárdenas Juan Diego

module Practica4 where

import Data.Char
import Data.List (nub)


data Token = Assign | If | Then | Else | Seq | While | Do | Skip | Boolean Bool | Equal | And | Not | Loc Int | Number Int | LP | RP | Sum deriving (Eq, Show)

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


data Content = T Token | S | C | B | E deriving (Eq, Show)
type Input = [Token]
type Stack = [Content]

parserAux :: Input -> Stack -> Bool
parserAux [] [] = True
parserAux [] l = False
parserAux l [] = False
parserAux (t1:xs) (T t2:ys)
  | t1 == t2 = parserAux xs ys
parserAux (x:xs) (y:ys)
  | entradaTabla /= [] = parserAux (x:xs) (entradaTabla ++ ys)
  | otherwise = False
  where
    entradaTabla = tablaAS x y

{- Ejemplo -}
-- parserAux [LP, Loc 2, Assign, Number 1, Seq, LP, Loc 3, Assign, Number 0, Seq, While, Not, Loc 2, Equal, Loc 2, Do, LP, Loc 2, Assign, LP, Loc 2, Sum, Number 1, RP, Seq, Loc 3, Assign, LP, Loc 3, Sum, Number 1, RP, RP, RP, RP] [S]
-- True
-- parserAux [ If , Not , And , Boolean True , Boolean False , Then , Skip , Else , Skip ] [ S ]
-- True

tablaAS :: Token -> Content -> Stack
tablaAS (Loc l) S = [C]
tablaAS If S = [C]
tablaAS LP S = [C]
tablaAS While S = [C]
tablaAS Skip S = [C]
tablaAS (Loc l) C = [T (Loc l), T Assign, E]
tablaAS If C = [T If, B, T Then, C, T Else, C]
tablaAS LP C = [T LP, C, T Seq, C, T RP]
tablaAS While C = [T While, B, T Do, C]
tablaAS Skip C = [T Skip]
tablaAS (Loc l) B = [E, T Equal, E]
tablaAS (Number n) B = [E, T Equal, E]
tablaAS LP B = [E, T Equal, E]
tablaAS (Boolean True) B = [T (Boolean True)]
tablaAS (Boolean False) B = [T (Boolean False)]
tablaAS And B = [T And, B, B]
tablaAS Not B = [T Not, B]
tablaAS (Loc l) E = [T (Loc l)]
tablaAS (Number n) E = [T (Number n)]
tablaAS LP E = [T LP, E, T Sum, E, T RP]
tablaAS _ _ = []

parser :: Input -> Bool
parser input = parserAux input [S]

{- Ejemplo -}
--parser [LP, Loc 2, Assign, Number 1, Seq, LP, Loc 3, Assign, Number 0, Seq, While, Not, Loc 2, Equal, Loc 2, Do, LP, Loc 2, Assign, LP, Loc 2, Sum, Number 1, RP, Seq, Loc 3, Assign, LP, Loc 3, Sum, Number 1, RP, RP, RP, RP]

data C = AssignASA E E | IfThenElse B C C | SeqASA C C | WhileDo B C | SkipASA deriving (Eq, Show)
data B = BoolASA Bool | EqualASA E E | AndASA B B | NotASA B deriving (Eq, Show)
data E = LocASA Int | NumberASA Int | SumASA E E deriving (Eq, Show)
