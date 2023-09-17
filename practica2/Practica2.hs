-- Datos del equipo:
-- 418002485 Cureño Sanchez Misael
-- 318224187 Bernal Núñez Raúl
-- 316641902 García Luna Bobadilla Uriel
-- 419002237 Jardón Cárdenas Juan Diego
module Practica2 where

import Data.Char
import Data.List (nub)

data Token
  = Var String
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
  where
    (num, rest) = span isDigit cs

lexAlph :: String -> [Token]
lexAlph cs = Var var : lexer rest
  where
    (var, rest) = span isAlpha cs

lexer :: String -> [Token]
lexer "" = []
lexer (' ':xs) = lexer xs
lexer ('+':xs) = Sum : (lexer xs)
lexer ('-':xs) = Subs : (lexer xs)
lexer ('&':'&':xs) = And : (lexer xs)
lexer ('|':'|':xs) = Or : (lexer xs)
lexer ('=':'=':xs) = Equal : (lexer xs)
lexer (x:xs)
  | isDigit x = lexNum (x : xs)
  | isAlpha x = lexAlph (x : xs)

-- Análisis sintáctico
data ASA
  = VarASA String
  | NumberASA Int
  | BooleanASA Bool
  | Op Token ASA ASA
  deriving (Show, Eq)

type Stack = [ASA]

-- Ejercicio 2
scannerAux :: [Token] -> Stack -> ASA
scannerAux [] [x] = x
scannerAux [x] [] =
  case x of
    Var     v -> VarASA     v
    Number  n -> NumberASA  n
    Boolean b -> BooleanASA b
scannerAux (x:xs) [] =
  case x of
    Var     v -> scannerAux xs [(VarASA     v)]
    Number  n -> scannerAux xs [(NumberASA  n)]
    Boolean b -> scannerAux xs [(BooleanASA b)]
scannerAux [] _ = error "Error de sintaxis"
scannerAux [x] (top:bottom) =
  case x of
    Var     v -> VarASA     v
    Number  n -> NumberASA  n
    Boolean b -> BooleanASA b
    Sum       -> Op Sum   top (head bottom)
    Subs      -> Op Subs  top (head bottom)
    And       -> Op And   top (head bottom)
    Or        -> Op Or    top (head bottom)
    Equal     -> Op Equal top (head bottom)
scannerAux (x:xs) (top:bottom) =
  case x of
    Var     v -> scannerAux xs (VarASA      v : (top:bottom))
    Number  n -> scannerAux xs (NumberASA   n : (top:bottom))
    Boolean b -> scannerAux xs (BooleanASA  b : (top:bottom))
    Sum       -> scannerAux xs (Op Sum   top (head bottom) : (drop 1 bottom))
    Subs      -> scannerAux xs (Op Subs  top (head bottom) : (drop 1 bottom))
    And       -> scannerAux xs (Op And   top (head bottom) : (drop 1 bottom))
    Or        -> scannerAux xs (Op Or    top (head bottom) : (drop 1 bottom))
    Equal     -> scannerAux xs (Op Equal top (head bottom) : (drop 1 bottom))

scanner :: [Token] -> ASA
scanner tokens = scannerAux tokens []

-- Análisis semántico
data Type
  = Num
  | Bool
  deriving (Show, Eq)

-- Ejercicio 3
typeCheckerAux :: ASA -> Type
typeCheckerAux (VarASA var) = Num
typeCheckerAux (NumberASA n) = Num
typeCheckerAux (BooleanASA b) = Bool
typeCheckerAux (Op t l r) =
  let typeL = typeCheckerAux l
      typeR = typeCheckerAux r
   in case t of
        n
          | n == Sum || n == Subs || n == Equal ->
            if typeL == typeR && typeR == Num
              then Num
              else error "Something went wrong"
        n
          | n == And || n == Or ->
            if typeL == typeR && typeR == Bool
              then Bool
              else error "Something went wrong"

-- typeCheckerAux (Op And (BooleanASA True) (Op Equal (VarASA "var") (Op Sum (NumberASA 3) (NumberASA 22)))) -- Bool
-- typeCheckerAux (Op And (NumberASA 43) (Op Equal (VarASA "var") (Op Sum (NumberASA 3) (NumberASA 22)))) 
-- El tipo de los argumentos NumberASA 43 y Op Equal ( VarASA " var " ) 
-- (Op Sum (NumberASA 3) (NumberASA 22)) no son los esperados para el operador And
-- Ejercicio 4
typeChecker :: ASA -> ASA
typeChecker asa =
  let t = typeCheckerAux asa
   in if t == Num || t == Bool
        then asa
        else error "Error lanzado desde typeChecker"

-- typeChecker (Op And (BooleanASA True) (Op Equal (VarASA "var") (Op Sum (NumberASA 3) (NumberASA 22)))) -- Devuelve el ASA si el tipado es consistente
-- Optimización de Código Fuente
-- Ejercicio 5
constantFolding :: ASA -> ASA
constantFolding _ = VarASA "var"

-- constantFolding (Op And (BooleanASA True) (Op Equal (VarASA "var") (Op Sum (NumberASA 3) (NumberASA 22))))
-- constantFolding (Op And (Op Equal (VarASA "var") (
data Value
  = N Int
  | B Bool
  | S String
  deriving (Eq)

instance Show Value where
  show (N n) = show n
  show (B b) = show b
  show (S s) = show s

data ThreeAddress
  = Assign String Value
  | Operation String String Token String
  deriving (Eq)

instance Show ThreeAddress where
  show (Assign t v) = show t ++ " = " ++ show v
  show (Operation t a op b) =
    show t ++ " = " ++ show a ++ tokenTreeAddress op ++ show b

tokenTreeAddress :: Token -> String
tokenTreeAddress t
  | t == Sum  = "+"
  | t == Subs = "-"
  | t == And  = "&&"
  | t == Or   = "||"
  | t == Equal = "=="
  | otherwise = error "Token not recognized"

-- Ejercicio 6
fresh :: [Int] -> Int
fresh [] = 0
fresh l = firstInRangeNotExistingInList [0 .. maximum l + 1] l
  where
    firstInRangeNotExistingInList :: [Int] -> [Int] -> Int
    firstInRangeNotExistingInList [last] l = last
    firstInRangeNotExistingInList (x:xs) l =
      if not (x `elem` l)
        then x
        else firstInRangeNotExistingInList xs l

-- Ejercicio 7
threeAddressAux :: ASA -> [Int] -> ([ThreeAddress], String, [Int])
threeAddressAux (VarASA var) usedTemps =
  let
    newTempVar = "t" ++ show (length usedTemps)
  in
    ([Assign newTempVar (S var)], newTempVar, length usedTemps : usedTemps)
threeAddressAux (NumberASA num) usedTemps =
  let
    newTempVar = "t" ++ show (length usedTemps)
  in
    ([Assign newTempVar (N num)], newTempVar, length usedTemps : usedTemps)
threeAddressAux (BooleanASA boolVal) usedTemps =
  let
    newTempVar = "t" ++ show (length usedTemps)
  in
    ([Assign newTempVar (B boolVal)], newTempVar, length usedTemps : usedTemps)
threeAddressAux (Op token left right) usedTemps =
  let
    (codeLeft, tempVarLeft, updatedTemps1) = threeAddressAux left usedTemps
    (codeRight, tempVarRight, updatedTemps2) = threeAddressAux right updatedTemps1
    newTempVar = "t" ++ show (length updatedTemps2)
    codeOp = case token of
      Sum -> [Operation newTempVar tempVarLeft Sum tempVarRight]
      Subs -> [Operation newTempVar tempVarLeft Subs tempVarRight]
      And -> [Operation newTempVar tempVarLeft And tempVarRight]
      Or -> [Operation newTempVar tempVarLeft Or tempVarRight]
      Equal -> [Operation newTempVar tempVarLeft Equal tempVarRight]
  in
    (codeLeft ++ codeRight ++ codeOp, newTempVar, length updatedTemps2 : updatedTemps2)

threeAddress :: ASA -> [ThreeAddress]
threeAddress asa = 
  let
    (three, _, _) = threeAddressAux asa []
    in
      three

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
compile = assembly . threeAddress . constantFolding . typeChecker . scanner . lexer
