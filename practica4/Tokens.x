{
module Tokens where
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
  \;                              { \s -> Seq }
  while                           { \s -> While }
  do                              { \s -> Do }
  skip                            { \s -> Skip }
  (true|false)                    { \s -> Boolean (s == "true") }
  =				                  { \s -> Equal }
  &				                  { \s -> And }
  \-				              { \s -> Not }
  $digit+                         { \s -> Number (read s :: Int) }
  L $digit*                       { \s -> Loc (read (tail s) :: Int) }
  \(				              { \s -> LP }
  \)				              { \s -> RP }
  \+ 				              { \s -> Sum }

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

lexer = alexScanTokens
}

