{
    module Lexer where
}

%wrapper "basic"

$digit = 0-9

tokens :-
    $white+     ;
    "+"         { \s -> TokenPlus }
    "-"         { \s -> TokenMinus }
    $digit+     { \s -> (read s :: Int) }

{
data Token = TokenPlus | TokenMinus | TokenInt Int deriving (Show)

parseError :: [Token] -> a
parseError _ = error "Parse error"

-- alex Tokens.x -o Lexer.hs
}
```