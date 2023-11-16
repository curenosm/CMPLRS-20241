import Practica5(Token(..), Content, State(..), Stack, Input, Symbols)
import Data.List
import Test.HUnit
import Control.Exception (evaluate, SomeException, try)


main = runTestTT (TestsList [])
