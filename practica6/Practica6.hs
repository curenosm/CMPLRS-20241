module Practica6 where

-- Datos del equipo:
-- 418002485 Cureño Sanchez Misael
-- 318224187 Bernal Núñez Raúl
-- 316641902 García Luna Bobadilla Uriel
-- 419002237 Jardón Cárdenas Juan Diego

data ASA = Assign ASA ASA
    | IfThenElse ASA ASA ASA
    | Seq ASA ASA
    | WhileDo ASA ASA
    | Skip
    | Boolean Bool
    | Equal ASA ASA
    | And ASA ASA
    | Not ASA
    | Loc Int
    | Number Int
    | Sum ASA ASA deriving Show

data Type = Num | Bool | Void deriving (Show, Eq)

checkType :: [ASA] -> Type -> Type -> Type
checkType xs t res = if all (==t) (map typeCheckerAux xs) then res else error "Not implemented"

typeCheckerAux :: ASA -> Type
typeCheckerAux (Number _)      = Num
typeCheckerAux (Boolean _)     = Bool
typeCheckerAux (Loc _)         = Num
typeCheckerAux (Skip)          = Void
typeCheckerAux (Not         e) = checkType [e] Bool Bool
typeCheckerAux (Assign    l v) = checkType [v] Num Void
typeCheckerAux (And       l r) = checkType [l, r] Bool Bool
typeCheckerAux (Equal     l r) = checkType [l, r] Num Bool
typeCheckerAux (Sum       l r) = checkType [l, r] Num Num
typeCheckerAux (Seq cur next)  = checkType [cur, next] Void Void
typeCheckerAux (IfThenElse c body other) = if typeCheckerAux c == Bool
    then checkType [body, other] Void Void else error "Not implemented"
typeCheckerAux (WhileDo c body)       = if typeCheckerAux c == Bool
    then checkType [body] Void Void else error "Not implemented"

typeChecker :: ASA -> ASA
typeChecker asa = if typeCheckerAux asa `elem` [Num, Bool, Void] 
    then asa 
    else error "Not implemented"

