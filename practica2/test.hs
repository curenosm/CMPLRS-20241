import Data.List
import Practica2
  ( ASA(BooleanASA, NumberASA, Op, VarASA)
  , Stack
  , Token(And, Equal, Number, Or, Subs, Sum, Var)
  , Type(Bool, Num)
  , Value(N, B, S)
  , ThreeAddress(Assign, Operation)
  , fresh
  , lexer
  , scanner
  , constantFolding
  , threeAddress
  , threeAddressAux
  , typeChecker
  , typeCheckerAux
  , chooseInstruction
  , assembly
  , compile
  )
import Test.HUnit

test_1 = TestCase (assertEqual "1" 1 1)

test_lexer_1 =
  TestCase
    (assertEqual
       "lexer '22 3 + var == t &&'"
       (lexer "22 3 + var == t &&")
       [Number 22, Number 3, Sum, Var "var", Equal, Var "t", And])

test_lexer_2 =
  TestCase
    (assertEqual
       "lexer '22 + var var f t == &&'"
       (lexer "22 + var var f t == &&")
       [Number 22, Sum, Var "var", Var "var", Var "f", Var "t", Equal, And])

test_scanner_1 =
  TestCase
    (assertEqual
       "scanner '22 3 + var == t &&'"
       (scanner (lexer "22 3 + var == t &&"))
       (Op
          And
          (VarASA "t")
          (Op Equal (VarASA "var") (Op Sum (NumberASA 3) (NumberASA 22)))))

test_scanner_2 =
  TestCase
    (assertEqual
       "scanner '2 2 + 2 =='"
       (scanner (lexer "2 2 + 2 =="))
       (Op Equal (NumberASA 2) (Op Sum (NumberASA 2) (NumberASA 2))))

test_constantFolding_1 =
  TestCase
    (assertEqual
       "constantFolding (Op And (BooleanASA True) (Op Equal (VarASA 'var') (Op Sum (NumberASA 3) (NumberASA 22))))"
       (constantFolding (Op And (BooleanASA True) (Op Equal (VarASA "var") (Op Sum (NumberASA 3) (NumberASA 22)))))
       (Op Equal (VarASA "var") (NumberASA 25)))

test_constantFolding_2 =
  TestCase
    (assertEqual
       "constantFolding (Op And (Op Equal (VarASA 'var') (VarASA 'var')) (Op Equal (VarASA 'var') (Op Sum (VarASA 'var') (NumberASA 22))))"
       (constantFolding (Op And (Op Equal (VarASA "var") (VarASA "var")) (Op Equal (VarASA "var") (Op Sum (VarASA "var") (NumberASA 22)))))
       (Op And (Op Equal (VarASA "var") (VarASA "var")) (Op Equal (VarASA "var") (Op Sum (VarASA "var") (NumberASA 22)))))

test_fresh_1 = TestCase (assertEqual "fresh [1, 2, 3]" (fresh [1, 2, 3]) 0)

test_fresh_2 =
  TestCase (assertEqual "fresh [4, 2, 3, 0]" (fresh [4, 2, 3, 0]) 1)

test_typeCheckerAux_1 =
  TestCase
    (assertEqual
       "typeCheckerAux (NumberASA 1)"
       (typeCheckerAux (NumberASA 1))
       Num)

test_typeCheckerAux_2 =
  TestCase
    (assertEqual
       "typeCheckerAux (BooleanASA True)"
       (typeCheckerAux (BooleanASA True))
       Bool)

test_typeCheckerAux_3 =
  TestCase
    (assertEqual
       "typeCheckerAux (Op Sum (NumberASA 1) (NumberASA 1)"
       (typeCheckerAux (Op Sum (NumberASA 1) (NumberASA 1)))
       Num)

test_typeCheckerAux_4 =
  TestCase
    (assertEqual
       "typeCheckerAux (Op And (BooleanASA True) (NumberASA 1))"
       (typeCheckerAux (Op And (BooleanASA True) (NumberASA 1)))
       (error "Something went wrong"))

test_typeChecker_1 =
  TestCase
    (assertEqual
       "typeChecker (Op Equal (Op Add (NumberASA 1) (NumberASA 2)) (Op Subs (NumberASA 3) (NumberASA 0)))"
       (typeChecker
          (Op
             Equal
             (Op Sum (NumberASA 1) (NumberASA 2))
             (Op Subs (NumberASA 3) (NumberASA 0))))
       (Op
          Equal
          (Op Sum (NumberASA 1) (NumberASA 2))
          (Op Subs (NumberASA 3) (NumberASA 0))))

test_typeChecker_2 =
  TestCase
    (assertEqual
       "typeChecker (Op And (Op Or (BooleanASA False) (BooleanASA True)) (Op Or (BooleanASA False) (BooleanASA False)))"
       (typeChecker
          (Op
             And
             (Op Or (BooleanASA False) (BooleanASA True))
             (Op Or (BooleanASA False) (BooleanASA False))))
       (Op
          And
          (Op Or (BooleanASA False) (BooleanASA True))
          (Op Or (BooleanASA False) (BooleanASA False))))

test_threeAddress_1 =
  TestCase
    (assertEqual
       "threeAddress (Op Equal (VarASA 'var') (NumberASA 25))"
       (threeAddress (Op Equal (VarASA "var") (NumberASA 25)))
       [Assign "t0" (S "var"), Assign "t1" (N 25), Operation "t2" "t0" Equal "t1"])

test_threeAddress_2 =
  TestCase
    (assertEqual
       "threeAddress (Op Equal (NumberASA 50) (VarASA 'var'))"
       (threeAddress (Op Equal (NumberASA 50) (VarASA "var")))
       [Assign "t0" (N 50), Assign "t1" (S "var"), Operation "t2" "t0" Equal "t1"])

test_assembly_1 =
  TestCase
    (assertEqual
       "assembly [Assign 't0' (S 'var'), Assign 't1' (N 25), Operation 't2' 't0' Equal 't1']"
       (assembly [Assign "t0" (S "var"), Assign "t1" (N 25), Operation "t2" "t0" Equal "t1"])
       ("MOV t0 \"var\"" ++ "\n" ++ "MOV t1 25" ++ "\n" ++ "EQ t2 t0 t1"))

test_assembly_2 =
  TestCase
    (assertEqual
       "assembly [Assign 't0' (N 50), Assign 't1' (S 'var'), Operation 't2' 't0' Equal 't1']"
       (assembly [Assign "t0" (N 50), Assign "t1" (S "var"), Operation "t2" "t0" Equal "t1"])
       ("MOV t0 50" ++ "\n" ++ "MOV t1 \"var\"" ++ "\n" ++ "EQ t2 t0 t1"))

tests =
  TestList
    [ test_1
    , test_lexer_1
    , test_lexer_2
    , test_scanner_1
    , test_scanner_2
    , test_constantFolding_1
    , test_constantFolding_2
    , test_fresh_1
    , test_fresh_2
    , test_typeCheckerAux_1
    , test_typeCheckerAux_2
    , test_typeCheckerAux_3
    , test_typeCheckerAux_4
    , test_typeChecker_1
    , test_typeChecker_2
    , test_threeAddress_1
    , test_threeAddress_2
    , test_assembly_1
    , test_assembly_2
    ]

main = runTestTT tests
