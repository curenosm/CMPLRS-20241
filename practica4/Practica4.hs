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
parserAux (x:xs) (T y:ys) | x == y = parserAux xs ys
parserAux (x:xs) (y:ys) = parserAux (x:xs) ((tablaAux x y) ++ ys)

{- Ejemplo -}
-- parserAux [LP, Loc 2, Assign, Number 1, Seq, LP, Loc 3, Assign, Number 0, Seq, While, Not, Loc 2, Equal, Loc 2, Do, LP, Loc 2, Assign, LP, Loc 2, Sum, Number 1, RP, Seq, Loc 3, Assign, LP, Loc 3, Sum, Number 1, RP, RP, RP, RP] [S]
-- True
-- parserAux [ If , Not , And , Boolean True , Boolean False , Then , Skip , Else , Skip ] [ S ]
-- True

tablaAux :: Token -> Content -> Stack
tablaAux (Loc l) S = [C]
tablaAux If S = [C]
tablaAux LP S = [C]
tablaAux While S = [C]
tablaAux Skip S = [C]
tablaAux (Loc l) C = [T (Loc l), T Assign, E]
tablaAux If C = [T If, B, T Then, C, T Else, C]
tablaAux LP C = [T LP, C, T Seq, C, T RP]
tablaAux While C = [T While, B, T Do, C]
tablaAux Skip C = [T Skip]
tablaAux (Loc l) B = [E, T Equal, E]
tablaAux (Number n) B = [E, T Equal, E]
tablaAux LP B = [E, T Equal, E]
tablaAux (Boolean True) B = [T (Boolean True)]
tablaAux (Boolean False) B = [T (Boolean False)]
tablaAux And B = [T And, B, B]
tablaAux Not B = [T Not, B]
tablaAux (Loc l) E = [T (Loc l)]
tablaAux (Number n) E = [T (Number n)]
tablaAux LP E = [T LP, E, T Sum, E, T RP]
tablaAux _ _ = []




-- TODO
--parser :: Input -> Bool
{- Ejemplo -}
--parser [LP, Loc 2, Assign, Number 1, Seq, LP, Loc 3, Assign, Number 0, Seq, While, Not, Loc 2, Equal, Loc 2, Do, LP, Loc 2, Assign, LP, Loc 2, Sum, Number 1, RP, Seq, Loc 3, Assign, LP, Loc 3, Sum, Number 1, RP, RP, RP, RP]

data C = AssignASA E E | IfThenElse B C C | SeqASA C C | WhileDo B C | SkipASA deriving (Eq, Show)
data B = BoolASA Bool | EqualASA E E | AndASA B B | NotASA B deriving (Eq, Show)
data E = LocASA Int | NumberASA Int | SumASA E E deriving (Eq, Show)
