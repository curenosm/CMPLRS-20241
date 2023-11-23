data Token = Assign | If | Then | Else | Seq | While | Do | Skip | Boolean Bool | Equal | And | Not | Loc Int | Number Int | LP | RP deriving Show 
data Content = T Token | S | C | PC | B | PB | E | PE deriving Show
data State = Q Int deriving Show

type Input = [Token]
type Stack = [State]
type Symbol = [Content]

parserAux :: Input -> Stack -> Symbol -> Bool
--SHIFT
--Verificamos el primer token del input
--Verificamos el estado en que nos encontramos
--Continuamos la derivacion transicionando al agregar un nuevo estado y el simbolo que leimos
parserAux (Skip:xs) (Q 0:ys) ws = parserAux xs (Q 4:Q 0:ys) (T Skip:ws)

--REDUCE 
--Verificamos el primer token del input (el simbolo de fin de cadena es equivalente a un input vacio)
--Verificamos el estado en el que nos encontramos
--Verificamos que los simbolos y estados que vamos a descartar son correctos
--Continuamos la derivacion reemplazando estados y simbolos
parserAux [] (Q 4:ys) (T Skip:ws) = parserAux [] (Q 2:ys) (PC:ws)
parserAux [] (Q 2:ys) (PC:ws) = parserAux [] (Q 1:ys) (C:ws)

--ACCEPT
parserAux [] (Q 1:ys) ws = True
