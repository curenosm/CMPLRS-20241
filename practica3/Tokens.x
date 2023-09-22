{
module Tokens (lexer, alexScanTokens, Token (
  Assign, If, Then, Else,
  Seq, While, Do, Skip,
  Boolean, Equal, And, 
  Not, Loc, Number, LP, RP, Sum)) where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+                         ; -- ignore white space
  :=                              { \s -> Assign }
  if                              { \s -> If }
  then                            { \s -> Then }
  else                            { \s -> Else }
  \;                               { \s -> Seq }
  while                           { \s -> While }
  do                              { \s -> Do }
  skip                            { \s -> Skip }
  (true|false)                    { \s -> Boolean (s == "true") }
  $digit+                         { \s -> Number (read s :: Int) }
  L $digit*                       { \s -> Loc (read (tail s) :: Int) }


{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = Assign
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
  | Sum
  deriving (Eq, Show)

lexer = do
  s <- getContents
  print (alexScanTokens s)
}