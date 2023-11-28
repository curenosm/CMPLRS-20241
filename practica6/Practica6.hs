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
typeCheckerAux (Loc _) = Num
typeCheckerAux (Number _) = Num
typeCheckerAux (Boolean _) = Bool
typeCheckerAux Skip = Void
typeCheckerAux (Sum a1 a2)
  | typeIsNum t1 && typeIsNum t2 = Num
  | otherwise = error "El tipo de los operandos es incorrecto."
  where
    t1 = typeCheckerAux a1
    t2 = typeCheckerAux a2
typeCheckerAux (And b1 b2)
  | typeIsBool t1 && typeIsBool t2 = Bool
  | otherwise = error "El tipo de los operandos es incorrecto."
  where
    t1 = typeCheckerAux b1
    t2 = typeCheckerAux b2
typeCheckerAux (Not b)
  | typeIsBool t = Bool
  | otherwise = error "El tipo del operando es incorrecto."
  where
    t = typeCheckerAux b
typeCheckerAux (Equal a1 a2)
  | typeIsNum t1 && typeIsNum t2 = Bool
  | otherwise = error "El tipo de los operandos es incorrecto."
  where
    t1 = typeCheckerAux a1
    t2 = typeCheckerAux a2
typeCheckerAux (Assign l a)
  | typeIsNum t = Void
  | otherwise = error "El tipo de los operandos es incorrecto."
  where
    t = typeCheckerAux a
typeCheckerAux (IfThenElse b c1 c2)
  | typeIsBool t1 && typeIsVoid t2 && typeIsVoid t3 = Void
  | otherwise = error "El tipo de los operandos es incorrecto."
  where
    t1 = typeCheckerAux b
    t2 = typeCheckerAux c1
    t3 = typeCheckerAux c2
typeCheckerAux (Seq c1 c2)
  | typeIsVoid t1 && typeIsVoid t2 = Void
  | otherwise = error "El tipo de los operandos es incorrecto."
  where
    t1 = typeCheckerAux c1
    t2 = typeCheckerAux c2
typeCheckerAux (WhileDo b c)
  | typeIsBool t1 && typeIsVoid t2 = Void
  | otherwise = error "El tipo de los operandos es incorrecto."
  where
    t1 = typeCheckerAux b
    t2 = typeCheckerAux c

isType :: Type -> Bool
isType t = typeIsBool t || typeIsNum t || typeIsVoid t

typeChecker :: ASA -> ASA
typeChecker (Assign l a)
  | isType t = (Assign l a)
  | otherwise = error ":("
  where
    t = typeCheckerAux (Assign l a)
typeChecker (IfThenElse b c1 c2)
  | isType t = (IfThenElse b c1 c2)
  | otherwise = error ":("
  where
    t = typeCheckerAux (IfThenElse b c1 c2)
typeChecker (Seq c1 c2)
  | isType t = (Seq c1 c2)
  | otherwise = error ":("
  where
    t = typeCheckerAux (Seq c1 c2)
typeChecker (WhileDo b c)
  | isType t = (WhileDo b c)
  | otherwise = error ":("
  where
    t = typeCheckerAux (WhileDo b c)
typeChecker Skip
  | isType t = Skip
  | otherwise = error ":("
  where
    t = typeCheckerAux Skip
typeChecker (Boolean b) = error "Los booleanos no son programas válidos en el lenguaje."
typeChecker (Equal a1 a2) = error "Las operaciones de Equal no son programas válidos en el lenguaje."
typeChecker (And a1 a2) = error "Las operaciones de AND no son programas válidos en el lenguaje."
typeChecker (Not b) = error "Las operaciones de NOT no son programas válidos en el lenguaje."
typeChecker (Loc l) = error "Las localidades no son programas válidos en el lenguaje."
typeChecker (Number n) = error "Los números no son programas válidos en el lenguaje."
typeChecker (Sum a1 a2) = error "Las sumas no son programas válidos en el lenguaje."


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
