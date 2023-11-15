-- Datos del equipo:
-- 418002485 Cureño Sanchez Misael
-- 318224187 Bernal Núñez Raúl
-- 316641902 García Luna Bobadilla Uriel
-- 419002237 Jardón Cárdenas Juan Diego

data ASA = Assign ASA ASA
    | IfThenElse ASA ASA ASA
    | Seq ASA ASA
    | WhileDo C C
    | Skip
    | Boolean Bool
    | Equal ASA ASA
    | And ASA ASA
    | Not ASA
    | Loc Int
    | Number Int
    | Sum ASA ASA deriving Show

data Type = Num | Bool | Void deriving Show

-- TODO: Define la función typeCheckerAux que recibe un ASA y devuelve
-- el tipo de la expresión únicamente si el tipado del programa es consistente
-- en otro arroja un error indicando el problema con el programa.
typeCheckerAux :: ASA -> Type
typeCheckerAux _ = error "Not implemented"
{- Ejemplo

typeCheckerAux (Sum (Loc 2) (Number 5))
Num
-}


-- TODO: Define la función typeChecker que recibe un ASA y devuelve dicho ASA
-- si el tipado del programa es consistente y el programa es admisible.
typeChecker :: ASA -> ASA
typeChecker _ = error "Not implemented"
{- Ejemplo

typeCheckerAux (Sum (Loc 2) (Number 5))
error: Las expresiones aritméticas no son programas válidos en el lenguaje.

typeCheckerAux (IfThenElse (Number 2) (Not (Number 5)) (Skip))
error: El tipo de (Number 2) no es el esperado.
-}
