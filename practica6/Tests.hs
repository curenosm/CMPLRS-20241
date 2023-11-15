import Practica6 (typeChecker, typeCheckerAux, ASA(..))
import Data.List
import Test.HUnit
import Control.Exception (evaluate, SomeException, try)


test_typeCheckerAux_1 = TestCase $ do
    result <- try (evaluate (typeCheckerAux (Op And (BooleanASA True) (NumberASA 1)))) :: IO (Either SomeException Type)
    case result of
        Left _  -> return () -- Success: An exception was raised
        Right _ -> assertFailure "Expected an error, but no error was raised."
        -- "error: Las expresiones aritméticas no son programas válidos en el lenguaje."

test_typeCheckerAux_2 = TestCase $ do
    result <- try (evaluate (typeCheckerAux (Op And (BooleanASA True) (NumberASA 1)))) :: IO (Either SomeException Type)
    case result of
        Left _  -> return () -- Success: An exception was raised
        Right _ -> assertFailure "Expected an error, but no error was raised."
        -- "error: El tipo de (Number 2) no es el esperado."

tests =
  TestList
    [ test_typeCheckerAux_1
    , test_typeCheckerAux_2
    ]

main = runTestTT tests
