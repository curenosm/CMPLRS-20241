-- Datos del equipo:
-- 418002485 Cureño Sanchez Misael
-- 318224187 Bernal Núñez Raúl
-- 316641902 García Luna Bobadilla Uriel
-- 419002237 Jardón Cárdenas Juan Diego

module Practica2 where

import Data.Char
import Data.List (nub)



-- Análisis léxico

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
          | n == Sum || n == Subs ->
            if typeL == typeR && typeR == Num
              then Num
              else error "Something went wrong"
        n
          | n == Equal ->
            if typeL == typeR && typeR == Num
              then Bool
              else error "Something went wrong"
        n
          | n == And || n == Or ->
            if typeL == typeR && typeR == Bool
              then Bool
              else error "Something went wrong"

-- Ejercicio 4
typeChecker :: ASA -> ASA
typeChecker asa =
  let t = typeCheckerAux asa
   in if t == Num || t == Bool
        then asa
        else error "Error lanzado desde typeChecker"



-- Optimización de Código Fuente

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

-- Ejercicio 5
constantFolding :: ASA -> ASA
constantFolding (VarASA s) = VarASA s
constantFolding (NumberASA n) = NumberASA n
constantFolding (BooleanASA b) = BooleanASA b
constantFolding op =
  case op of
    Op Sum (NumberASA n1) (NumberASA n2) -> NumberASA (n1 + n2)
    Op Subs (NumberASA n1) (NumberASA n2) -> NumberASA (n1 - n2)
    Op Or (BooleanASA True) _ -> BooleanASA True
    Op Or _ (BooleanASA True) -> BooleanASA True
    Op Or (BooleanASA False) (BooleanASA False) -> BooleanASA False
    Op Or (BooleanASA False) e -> constantFolding e
    Op Or e (BooleanASA False) -> constantFolding e
    Op And (BooleanASA False) _ -> BooleanASA False
    Op And _ (BooleanASA False) -> BooleanASA False
    Op And (BooleanASA True) (BooleanASA True) -> BooleanASA True
    Op And (BooleanASA True) e -> constantFolding e
    Op And e (BooleanASA True) -> constantFolding e
    Op Equal (NumberASA n1) (NumberASA n2) -> BooleanASA (n1 == n2)
    Op Equal e1 e2 -> Op Equal (constantFolding e1) (constantFolding e2)
    asa -> asa

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
threeAddressAux (VarASA v) past_ids = (dirs, temp, i:past_ids)
  where
    i = fresh past_ids
    temp = "t" ++ show i
    dirs = [Assign temp (S v)]

threeAddressAux (BooleanASA b) past_ids = (dirs, temp, i:past_ids)
  where
    i = fresh past_ids
    temp = "t" ++ show i
    dirs = [Assign temp (B b)]

threeAddressAux (NumberASA n) past_ids = (dirs, temp, i:past_ids)
  where
    i = fresh past_ids
    temp = "t" ++ show i
    dirs = [Assign temp (N n)]

threeAddressAux (Op op l r) past_ids = (l' ++ r' ++ dirs, rt, rpst)
  where
    (l', lt, lpst) = threeAddressAux l past_ids
    (r', rt, rpst) = threeAddressAux r lpst
    i = fresh rpst
    temp = "t" ++ show i
    dirs = [Operation temp lt op rt]

threeAddress :: ASA -> [ThreeAddress]
threeAddress program = dirs
  where
    (dirs, t, prev_ids) = threeAddressAux program []



-- Generación de código
chooseInstruction :: Token -> String
chooseInstruction t
  | t == Sum = "ADD"
  | t == Subs = "SUB"
  | t == And = "AND"
  | t == Or = "OR"
  | t == Equal = "EQ"
  | otherwise = error "Token not recognized"

-- Ejercicio 8
assembly :: [ThreeAddress] -> String
assembly [] = ""
assembly [x] = case x of
  Assign t (N n) -> "MOV " ++ show n ++ " " ++ t
  Assign t (S s) -> "MOV " ++ show s ++ " " ++ t
  Assign t (B b) -> "MOV " ++ show b ++ " " ++ t
  Operation t a op b -> (chooseInstruction op) ++ " " ++ t ++ " " ++ a ++ " " ++ b
assembly (x:xs) = case x of
  Assign t (N n) -> "MOV " ++ t ++ " " ++ show n ++ "\n" ++ assembly xs
  Assign t (S s) -> "MOV " ++ t ++ " " ++ show s ++ "\n" ++ assembly xs
  Assign t (B b) -> "MOV " ++ t ++ " " ++ show b ++ "\n" ++ assembly xs
  Operation t a op b -> (chooseInstruction op) ++ " " ++ t ++ " " ++ a ++ " " ++ b ++ "\n" ++ assembly xs


-- Ejercicio Extra
compile :: String -> String
compile = assembly . threeAddress . constantFolding . typeChecker . scanner . lexer
