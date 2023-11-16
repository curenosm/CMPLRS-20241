import Data.List
import Tokens (lexer, Token(..)) -- Tokens.hs
import Test.HUnit
import Control.Exception (evaluate, SomeException, try)


main = runTestTT (TestList [])
