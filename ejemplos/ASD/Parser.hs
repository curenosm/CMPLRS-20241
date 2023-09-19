module Parse where

import Lexer

data ASA = NumNode Int
        | AddNode ASA ASA
        | SubNode ASA ASA
        deriving (Show)

parseExpr :: [Token] -> (ASA, [Token])
parseExpr tokens = 
    let (leftExp, restTokens1 ) = parseTerm tokens
    in case restTokens1 of
        (TokenPlus:restTokens2) -> 
            let (rightExp, restTokens3) = parseExpr restTokens2
            in (AddNode leftExp rightExp, restTokens3)
        (TokenMinus:restTokens2) -> 
            let (rightExp, restTokens3) = parseExpr restTokens2
            in (SubNode leftExp rightExp, restTokens3)
        _ -> (leftExp, restTokens1)

parseTerm :: [Token] -> (ASA, [Token])

parse :: [Token] -> ASA
parse tokens = 
    let (expr, restTokens) = parseExpr tokens
    in if null restTokens
        then expr
        else error "Error de sintaxis: Entrada no válida."

main :: IO ()
main = do
    let tokens = [TokenNumber 2, TokenPlus, TokenNumber 3, TokenMinus, TokenNumber 1]
    let exp = parse tokens
    putStrLn ("Expresión analizada: " ++ show expr)
