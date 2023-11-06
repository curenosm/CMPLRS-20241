-- Datos del equipo:
-- 418002485 Cureño Sanchez Misael
-- 318224187 Bernal Núñez Raúl
-- 316641902 García Luna Bobadilla Uriel
-- 419002237 Jardón Cárdenas Juan Diego


data Token =  Assign
    | If
    | Then
    | Else
    | Seq
    | While
    | Do
    | Skip
    | Boolean Bool
    | Equal
    | And
    | Not
    | Loc Int
    | Number Int
    | LP
    | RP
    | Sum deriving Show

data Content = T Token | S | C | PC | B | PB | E | PE deriving Show
data State = Q Int deriving Show

type Input = [Token]
type Stack = [State]
type Symbols = [Content]

-- TODO: Implementar
parserAux :: Input -> Stack -> Symbols -> Bool
--Shift
parserAux (Loc l : xs) (Q 0 : ys) ws = parserAux xs (Q 3 : Q 0 : ys) (T (Loc l) : ws)
parserAux (Skip : xs) (Q 0 : ys) ws = parserAux xs (Q 4 : Q 0 : ys) (T Skip : ws)
parserAux (Seq : xs) (Q 2 : ys) ws = parserAux xs (Q 5 : Q 2 : ys) (T Seq : ws)
parserAux (Assign : xs) (Q 3 : ys) ws = parserAux xs (Q 6 : Q 3 : ys) (T Assign : ws)
parserAux (Loc l : xs) (Q 5 : ys) ws = parserAux xs (Q 3 : Q 5 : ys) (T (Loc l) : ws)
parserAux (Skip : xs) (Q 5 : ys) ws = parserAux xs (Q 4 : Q 5 : ys) (T Skip : ws)
parserAux (Loc l : xs) (Q 6 : ys) ws = parserAux xs (Q 10 : Q 6 : ys) (T (Loc l) : ws)
parserAux (Number n : xs) (Q 6 : ys) ws = parserAux xs (Q 11 : Q 6 : ys) (T (Number n) : ws)
parserAux (Sum : xs) (Q 9 : ys) ws = parserAux xs (Q 12 : Q 9 : ys) (T Sum : ws)
parserAux (Loc l : xs) (Q 12 : ys) ws = parserAux xs (Q 10 : Q 12 : ys) (T (Loc l) : ws)
parserAux (Number n : xs) (Q 12 : ys) ws = parserAux xs (Q 11 : Q 12 : ys) (T (Number n) : ws)
--Reduce
parserAux [] (Q 4 : ys) (T Skip : ws) = parserAux [] (Q 2 : ys) (PC : ws)
parserAux [] (Q 2 : ys) (PC : ws ) = parserAux [] (Q 1 : ys) (C : ws)
--Accept
parserAux [] (Q 1 : ys) ws = True
parserAux _ _ _ = False

{- Ejemplo -}
-- parserAux [Loc 2
--     , Assign, Number 1, Seq, Loc 3, Assign, Number 0, Seq
--     , Number 0, Seq, While, Not, Loc 2, Equals, Loc 2, Do
--     , Loc 2, Assign , LP, Loc 2, Sum, Number 1, Seq, Loc 3
--     , Assign, LP, Loc 3, Sum, Number 1
--     ] [Q 0] [] -- False

-- parserAux [Loc 1
--         , Assign, Number 1, Seq, Loc 2
--         , Assign, Number 2, Seq, Skip
--         ] [Q 0] [] -- True

-- TODO: Implementar
-- parser :: Input -> Bool