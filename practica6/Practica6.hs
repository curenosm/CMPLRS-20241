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

data Type = Num | Bool | Void deriving Show

{-checkType :: [ASA] -> Type -> Type -> Type
checkType xs t res = if all (==t) (map typeCheckerAux xs) then res else error "Not implemented"-}

typeCheckerEqual :: Type -> Type -> Bool
typeCheckerEqual Num Num = True
typeCheckerEqual Bool Bool = True
typeCheckerEqual Void Void = True
typeCheckerEqual _ _ = False

typeIsBool :: Type -> Bool
typeIsBool Bool = True
typeIsBool _ = False

typeIsNum :: Type -> Bool
typeIsNum Num = True
typeIsNum _ = False

typeIsVoid :: Type -> Bool
typeIsVoid Void = True
typeIsVoid _ = False

typeCheckerAux :: ASA -> Type
typeCheckerAux (Loc _)         = Num
typeCheckerAux (Number _)      = Num
typeCheckerAux (Boolean _)     = Bool
typeCheckerAux Skip          = Void
typeCheckerAux (Sum a1 a2)
  | typeCheckerEqual t1 t2 && typeIsNum t1 = Num
  | otherwise = error "El tipo de los operandos es incorrecto."
  where
    t1 = typeCheckerAux a1
    t2 = typeCheckerAux a2
typeCheckerAux (And b1 b2)
  | typeCheckerEqual t1 t2 && typeIsBool t1 = Bool
  | otherwise = error "El tipo de los operandos es incorrecto."
  where
    t1 = typeCheckerAux b1
    t2 = typeCheckerAux b2
typeCheckerAux (Not b1)
  | typeIsBool t1 = Bool
  | otherwise = error "El tipo del operando es incorrecto."
  where
    t1 = typeCheckerAux b1
typeCheckerAux (Equal a1 a2)
  | typeCheckerEqual t1 t2 && typeIsNum t1 = Bool
  | otherwise = error "El tipo de los operandos es incorrecto."
  where
    t1 = typeCheckerAux a1
    t2 = typeCheckerAux a2

{-
typeCheckerAux (Not         e) = checkType [e] Bool Bool
typeCheckerAux (Assign    l v) = checkType [v] Num Void
typeCheckerAux (And       l r) = checkType [l, r] Bool Bool
typeCheckerAux (Equal     l r) = checkType [l, r] Num Bool
typeCheckerAux (Sum       l r) = checkType [l, r] Num Num
typeCheckerAux (Seq cur next)  = checkType [cur, next] Void Void
typeCheckerAux (IfThenElse c body other) = if typeCheckerAux c == Bool
    then checkType [body, other] Void Void else error "Not implemented"
typeCheckerAux (WhileDo c body)       = if typeCheckerAux c == Bool
    then checkType [body] Void Void else error "Not implemented"-}



{-typeChecker :: ASA -> ASA
typeChecker asa = if typeCheckerAux asa `elem` [Num, Bool, Void] 
    then asa 
    else error "Not implemented"
-}
