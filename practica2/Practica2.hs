-- Datos del equipo:
-- 418002485 Cureño Sanchez Misael
-- 318224187 Bernal Núñez Raúl

module Practica2 where

import Data.Char
import Data.List (nub)


data Token = Var String 
  | Number Int 
  | Boolean Bool 
  | Sum 
  | Subs 
  | And 
  | Or 
  | Equal 
  deriving (Show, Eq)


-- Análisis léxico

-- Ejercicio 1
lexNum :: String -> [Token]
lexNum cs = Number (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexAlph :: String -> [Token]
lexAlph cs = Var var : lexer rest
      where (var,rest) = span isAlpha cs

lexer :: String -> [Token]
lexer "" = []
lexer (' ':xs) = lexer xs
lexer ('+':xs) = Sum:(lexer xs)
lexer ('-':xs) = Subs:(lexer xs)
lexer ('&':'&':xs) = And:(lexer xs)
lexer ('|':'|':xs) = Or:(lexer xs)
lexer ('=':'=':xs) = Equal:(lexer xs)
lexer (x:xs)
  | isDigit x = lexNum (x:xs)
  | isAlpha x = lexAlph (x:xs)
  


-- Análisis sintáctico

data ASA = VarASA String 
  | NumberASA Int
  | BooleanASA Bool
  | Op Token ASA ASA
  deriving Show

type Stack = [ASA]

-- Ejercicio 2
scannerAux :: [Token] -> Stack -> ASA
scannerAux _ stack = VarASA "var"
-- scannerAux [Nunber 22, Number 3, Sum 3,Sum, Var "var", Equal, Boolean True, And] []
-- scannerAux [Number 22, Sum, Var "var", Var "var", Boolean False, Boolean True, Equals, And] [] -- Expresion mal formada


scanner :: [Token] -> ASA
scanner _ = VarASA "var"
-- scanner [Number 22, Number 3, Sum, Var "var", Equal, Boolean True, And]
-- scanner [Number 22, Sum, Var "var", Var "var", Boolean False, Boolean True, Equal, And] -- Expresión mal formada





-- Análisis semántico
data Type = Num | Bool deriving Show


-- Ejercicio 3
typeCheckerAux :: ASA -> Type
typeCheckerAux _ = Num
-- typeCheckerAux (Op And (BooleanASA True) (Op Equal (VarASA "var") (Op Sum (NumberASA 3) (NumberASA 22)))) -- Bool
-- typeCheckerAux (Op And (NumberASA 43) (Op Equal (VarASA "var") (Op Sum (NumberASA 3) (NumberASA 22)))) 
-- El tipo de los argumentos NumberASA 43 y Op Equal ( VarASA " var " ) 
-- (Op Sum (NumberASA 3) (NumberASA 22)) no son los esperados para el operador And


-- Ejercicio 4
typeChecker :: ASA -> ASA
typeChecker _ = VarASA "var"
-- typeChecker (Op And (BooleanASA True) (Op Equal (VarASA "var") (Op Sum (NumberASA 3) (NumberASA 22)))) -- Devuelve el ASA si el tipado es consistente





-- Optimización de Código Fuente

-- Ejercicio 5
constantFolding :: ASA -> ASA
constantFolding _ = VarASA "var"
-- constantFolding (Op And (BooleanASA True) (Op Equal (VarASA "var") (Op Sum (NumberASA 3) (NumberASA 22))))
-- constantFolding (Op And (Op Equal (VarASA "var") (VarASA "var")) (Op Equal (VarASA "var") (Op Sum (VarASA "var") (NumberASA 22))))


data Value = N Int | B Bool | S String 
instance Show Value 
    where
        show (N n) = show n
        show (B b) = show b
        show (S s) = show s


data ThreeAddress = Assign String Value | Operation String String Token String
instance Show ThreeAddress 
    where
        show (Assign t v) = show t ++ " = " ++ show v
        show (Operation t a op b) = show t ++ " = " ++ show a ++ show t ++ show op ++ show b


-- Ejercicio 6
fresh :: [Int] -> Int
fresh _ = 0
-- fresh [1, 2, 3] -- 0
-- fresh [4, 2, 3, 0] -- 1


-- Ejercicio 7
threeAddressAux :: ASA -> [Int] -> ([ThreeAddress], String, [Int])
threeAddressAux _ _ = ([], "t", [])

-- threeAddressAux (Op Equal (VarASA "var") (NumberASA 25)) []
-- (["t0" = "var", "t1" = 25, "t2" = "t0" == "t1" ], "t2", [2, 1, 0])

-- threeAddressAux (Op Equal (NumberASA 50) (VarASA "var")) []
-- (["t0" = 50, "t1" = "var", "t2" = "t0" == "t1"], "t2", [2, 1, 0])





-- Generación de código

-- Ejercicio 8
assembly :: [ThreeAddress] -> String
assembly _ = ""

-- assembly ["t0" = "var", "t1" = 25, "t2" = "t0" == "t1"]
-- MOV "t0" "var"
-- MOV "t1" 25
-- EQ "t2" "t0" "t1"

-- assembly ["t0" = 50, "t1" = "var", "t2" = "t0" == "t1"]
-- MOV "t0" 50
-- MOV "t1" "var"
-- EQ "t2" "t0" "t1"


-- Ejercicio Extra
compile :: String -> String
compile _ = ""

-- compile "22 3 + var == t &&"
-- MOV "t0" "var"
-- MOV "t1" 25
-- EQ "t2" "t0" "t1"
