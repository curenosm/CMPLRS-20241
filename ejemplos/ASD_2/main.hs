-- Gramática
-- S -> b A | c
-- A -> d S d | e

s :: String -> Bool
s (t:ts)
    | t == 'b' = a ts
    | t == 'c' && ts == "" = True
    | otherwise = False

a :: String -> Bool
a (t:ts) 
    | t == 'd' = if (s (init ts)) then (last ts) == 'd' else False
    | t == 'e' && ts == "" = True
    | otherwise = False

-- s "bdcd"
