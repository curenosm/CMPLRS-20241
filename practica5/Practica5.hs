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
    | Sum deriving (Eq, Show)

data Content = T Token | S | C | B | E deriving (Eq, Show)
data State = Q Int deriving (Eq, Show)

type Input = [Token]
type Stack = [State]
type Symbols = [Content]

-- TODO: Implementar
parserAux :: Input -> Stack -> Symbols -> Bool

{- Ejemplo -}
parserAux [Loc 2
    , Assign, Number 1, Seq, Loc 3, Assign, Number 0, Seq
    , Number 0, Seq, While, Not, Loc 2, Equals, Loc 2, Do
    , Loc 2, Assign , LP, Loc 2, Sum, Number 1, Seq, Loc 3
    , Assign, LP, Loc 3, Sum, Number 1
    ] [Q 0] [] -- False

parserAux [Loc 1
        , Assign, Number 1, Seq, Loc 2
        , Assign, Number 2, Seq, Skip
        ] [Q 0] [] -- True

-- TODO: Implementar
parser :: Input -> Bool