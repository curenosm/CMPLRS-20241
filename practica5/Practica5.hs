-- Datos del equipo:
-- 318224187 Bernal Núñez Raúl
-- 418002485 Cureño Sanchez Misael
-- 316641902 García Luna Bobadilla Uriel
-- 419002237 Jardón Cárdenas Juan Diego

module Practica5 where

data Token = Assign | If | Then | Else | Seq | While | Do | Skip | Boolean Bool | Equal | And | Not | Loc Int | Number Int | Sum | LP | RP deriving Show
data Content = T Token | S | C | PC | B | PB | E | PE deriving Show
data State = Q Int deriving Show

type Input = [Token]
type Stack = [State]
type Symbols = [Content]

parserAux :: Input -> Stack -> Symbols -> Bool

-- Desplazamiento
parserAux (Loc l:xs) (Q 0:ys) ws = parserAux xs (Q 3:Q 0:ys) (T (Loc l):ws)
parserAux (Skip:xs) (Q 0:ys) ws = parserAux xs (Q 4:Q 0:ys) (T Skip:ws)
parserAux (Seq:xs) (Q 2:ys) ws = parserAux xs (Q 5:Q 2:ys) (T Seq:ws)
parserAux (Assign:xs) (Q 3:ys) ws = parserAux xs (Q 6:Q 3:ys) (T Assign:ws)
parserAux (Loc l:xs) (Q 5:ys) ws = parserAux xs (Q 3:Q 5:ys) (T (Loc l):ws)
parserAux (Skip:xs) (Q 5:ys) ws = parserAux xs (Q 4:Q 5:ys) (T Skip:ws)
parserAux (Loc l:xs) (Q 6:ys) ws = parserAux xs (Q 10:Q 6:ys) (T (Loc l):ws)
parserAux (Number n:xs) (Q 6:ys) ws = parserAux xs (Q 11:Q 6:ys) (T (Number n):ws)
parserAux (Sum:xs) (Q 9:ys) ws = parserAux xs (Q 12:Q 9:ys) (T Sum:ws)
parserAux (Loc l:xs) (Q 12:ys) ws = parserAux xs (Q 10:Q 12:ys) (T (Loc l):ws)
parserAux (Number n:xs) (Q 12:ys) ws = parserAux xs (Q 11:Q 12:ys) (T (Number n):ws)

-- Reducción
parserAux [] (Q 2:Q 0:ys) (PC:ws ) = parserAux [] (Q 1:Q 0:ys) (C:ws)
parserAux [] (Q 2:Q 5:ys) (PC:ws ) = parserAux [] (Q 7:Q 5:ys) (C:ws)
parserAux [] (Q 4:ys) (T Skip:ws) = parserAux [] (Q 2:ys) (PC:ws)
parserAux [] (Q 7:Q 5:Q 2:Q 0:ys) (C:T Seq:PC:ws) = parserAux [] (Q 1:Q 0:ys) (C:ws)
parserAux [] (Q 7:Q 5:Q 2:Q 5:ys) (C:T Seq:PC:ws) = parserAux [] (Q 7:Q 5:ys) (C:ws)
--parserAux [] (Q 8:ys) (E:T Assign:T (Loc l):ws) = parserAux [] (Q 2:ys) (PC:ws)
--parserAux [] (Q 8:Q 6:Q 3:Q 0:ys) (E:T Assign:T (Loc l):ws) = parserAux [] (Q 2:Q 0:ys) (PC:ws)
--parserAux [] (Q 8:Q 6:Q 3:Q 5:ys) (E:T Assign:T (Loc l):ws) = parserAux [] (Q 2:Q 5:ys) (PC:ws)
parserAux [] (Q 8:Q 6:Q 3:ys) (E:T Assign:T (Loc l):ws) = parserAux [] (Q 2:ys) (PC:ws)
--parserAux [] (Q 9:ys) (PE:ws) = parserAux [] (Q 8:ys) (E:ws) || parserAux [] (Q 13:ys) (E:ws)
parserAux [] (Q 9:Q 6:ys) (PE:ws) = parserAux [] (Q 8:Q 6:ys) (E:ws)
parserAux [] (Q 9:Q 12:ys) (PE:ws) = parserAux [] (Q 13:Q 12:ys) (E:ws)

--parserAux [] (Q 10:Q 6:ys) (T (Loc l):ws) = parserAux [] (Q 9:Q 6:ys) (PE:ws)
--parserAux [] (Q 10:Q 12:ys) (T (Loc l):ws) = parserAux [] (Q 9:Q 12:ys) (PE:ws)
parserAux [] (Q 10:ys) (T (Loc l):ws) = parserAux [] (Q 9:ys) (PE:ws)
parserAux [] (Q 11:ys) (T (Number n):ws) = parserAux [] (Q 9:ys) (PE:ws)
--parserAux [] (Q 13:ys) (E:T Sum:PE:ws) = parserAux [] (Q 8:ys) (E:ws) || parserAux [] (Q 13:ys) (E:ws)
--parserAux [] (Q 13:Q 12:Q 9:Q 6:ys) (E:T Sum:PE:ws) = parserAux [] (Q 8:Q 12:Q 9:Q 6:ys) (E:ws)
parserAux [] (Q 13:Q 12:Q 9:Q 6:ys) (E:T Sum:PE:ws) = parserAux [] (Q 8:Q 6:ys) (E:ws)
--parserAux [] (Q 13:Q 12:Q 9:Q 12:ys) (E:T Sum:PE:ws) = parserAux [] (Q 13:Q 12:Q 9:Q 12:ys) (E:ws)
parserAux [] (Q 13:Q 12:Q 9:Q 12:ys) (E:T Sum:PE:ws) = parserAux [] (Q 13:Q 12:ys) (E:ws)

parserAux (Sum:xs) (Q 10:ys) (T (Loc l):ws) = parserAux (Sum:xs) (Q 9:ys) (PE:ws)
parserAux (Sum:xs) (Q 11:ys) (T (Number n):ws) = parserAux (Sum:xs) (Q 9:ys) (PE:ws)

parserAux (Seq:xs) (Q 4:ys) (T Skip:ws) = parserAux (Seq:xs) (Q 2:ys) (PC:ws)
--parserAux (Seq:xs) (Q 8:ys) (E:T Assign:T (Loc l):ws) = parserAux (Seq:xs) (Q 2:ys) (PC:ws)
--parserAux (Seq:xs) (Q 8:Q 6:Q 3:Q 0:ys) (E:T Assign:T (Loc l):ws) = parserAux (Seq:xs) (Q 2:Q 0:ys) (PC:ws)
--parserAux (Seq:xs) (Q 8:Q 6:Q 3:Q 5:ys) (E:T Assign:T (Loc l):ws) = parserAux (Seq:xs) (Q 2:Q 5:ys) (PC:ws)
parserAux (Seq:xs) (Q 8:Q 6:Q 3:ys) (E:T Assign:T (Loc l):ws) = parserAux (Seq:xs) (Q 2:ys) (PC:ws)--
--parserAux (Seq:xs) (Q 9:ys) (PE:ws) = parserAux (Seq:xs) (Q 8:ys) (E:ws) || parserAux [] (Q 13:ys) (E:ws)
parserAux (Seq:xs) (Q 9:Q 6:ys) (PE:ws) = parserAux (Seq:xs) (Q 8:Q 6:ys) (E:ws)
parserAux (Seq:xs) (Q 9:Q 12:ys) (PE:ws) = parserAux (Seq:xs) (Q 13:Q 12:ys) (E:ws)

parserAux (Seq:xs) (Q 10:ys) (T (Loc l):ws) = parserAux (Seq:xs) (Q 9:ys) (PE:ws)
parserAux (Seq:xs) (Q 11:ys) (T (Number n):ws) = parserAux (Seq:xs) (Q 9:ys) (PE:ws)
--parserAux (Seq:xs) (Q 13:ys) (E:T Sum:PE:ws) = parserAux (Seq:xs) (Q 8:ys) (E:ws) || parserAux (Seq:xs) (Q 13:ys) (E:ws)
parserAux (Seq:xs) (Q 13:Q 12:Q 9:Q 6:ys) (E:T Sum:PE:ws) = parserAux (Seq:xs) (Q 8:Q 6:ys) (E:ws)
parserAux (Seq:xs) (Q 13:Q 12:Q 9:Q 12:ys) (E:T Sum:PE:ws) = parserAux (Seq:xs) (Q 13:Q 12:ys) (E:ws)

-- Aceptación
parserAux [] (Q 1:ys) ws = True

-- Rechazo
parserAux _ _ _ = False


parser :: Input -> Bool
parser input = parserAux input [Q 0] []


-- Ejemplos

-- parser [Loc 1, Assign, Number 1, Seq, Loc 2, Assign, Number 2, Seq, Skip]
-- True

-- parser [Loc 1, Assign, Number 1, Seq, Loc 2, Assign, Number 2, Seq, Loc 3, Assign, Number 8, Sum, Loc 5, Sum, Number 15]
-- True

-- L1 := 1 ; L2 := 2 ; L3 := 8 + L5 + 15

-- parser [Loc 1, Assign, Number 1, Sum, Loc 2, Seq, Loc 3, Assign, Number 3]
-- True

-- L1 := 1 + L2 ; L3 := 3


-- parser [Loc 1, Assign, Loc 4, Seq, Loc 2, Assign, Number 2, Seq, Loc 3, Assign, Number 8, Sum, Loc 5, Sum, Number 15]
-- True

-- parser [Loc 2, Assign, Number 1, Seq, Loc 3, Assign, Number 0, Seq, While, Not, Loc 2, Equal, Loc 2, Do, Loc 2, Assign, LP, Loc 2, Sum, Number 1, Seq, Loc 3, Assign, LP, Loc 3, Sum, Number 1]
-- False

-- parser [Loc 1, Assign, Number 1, Sum, Loc 2, Sum, Loc 5, Seq, Loc 3, Assign, Number 3, Seq, Skip]
-- True

-- parser [Loc 1, Assign, Number 1, Sum, Loc 2, Seq, Loc 3, Assign, Number 3]
-- True
