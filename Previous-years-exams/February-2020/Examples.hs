----------------------------------------------------------------------
--
-- Examples.hs
-- Programming Languages
-- Fall 2020
--
-- Author: Pablo López
--- -------------------------------------------------------------------

module Examples where

import           NaturalSemanticsWhile20
import           While20

-- -------------------------------------------------------------------
-- Examples
-- -------------------------------------------------------------------

-- Problema 1 - Semántica natural de Aexp y Bexp

-- |
-- >> aVal initEnvV let1 initStore
-- 12

let0 :: Aexp
let0 =
  Let
      (Dec "x" (N 3) -- 3
      (Dec "y" (Mult (N 2) (V "x")) -- 6
      (Dec "z" (Add (V "x") (V "y")) -- 9
       EndDec)))

      (V "x") -- 12

let1 :: Aexp
let1 =

     Let
         (Dec "x" (N 3) -- 3
         (Dec "y" (Mult (N 2) (V "x")) -- 6
         (Dec "z" (Add (V "x") (V "y")) -- 9
          EndDec)))

         (Add (V "x") (V "z")) -- 12

-- |
-- >> aVal initEnvV nestedLet initStore
-- -9

nestedLet :: Aexp
nestedLet =

     Let
         (Dec "x" (N 1) -- 1
         (Dec "y" (Let  -- 10
                       (Dec "x" (N 5) -- 5
                       (Dec "y" (N 2) -- 2
                        EndDec))
                       (Mult (V "x") (V "y"))) -- 10
          EndDec))

         (Sub (V "x") (V "y")) -- -9

-- |
-- >> aVal initEnvV wrongLet1 initStore
-- *** Exception: undefined variable x

wrongLet1 :: Aexp
wrongLet1 =

     Add
        (Let
            (Dec "x" (N 1)
             EndDec)
            (V "x"))
         (V "x") -- error

-- |
-- >> aVal initEnvV wrongNestedLet initStore
-- *** Exception: undefined variable w

wrongNestedLet :: Aexp
wrongNestedLet =

     Let
         (Dec "x" (N 1) -- 1
         (Dec "y" (Let  -- 10
                       (Dec "x" (N 5) -- 5
                       (Dec "w" (N 2) -- 2
                        EndDec))
                       (Mult (V "x") (V "y"))) -- 10
          EndDec))

         (Sub (V "x") (V "w")) -- error

-- Problema 2.a - Programa con variables globales

-- |
-- >> showFinalState factorial
-- Program Factorial executed.
-- Global variables:
--	variable: x location: 1 value: 0
-- 	variable: y location: 2 value: 6
-- Number of cells used: 2
-- Memory dump: (1,0)(2,6)

factorial :: Program
factorial =

     Program "Factorial"

        (Dec "x" (N 3)  -- Global Variables
        (Dec "y" (N 1)
         EndDec))

        (While (Neg (Eq (V "x" ) (N 0)))
            (Comp
                (Ass "y" (Mult (V "y") (V "x")))
                (Ass "x" (Sub (V "x") (N 1)))))

-- |
-- >> showFinalState factorialWithLocal
-- Program FactorialWithLocal executed.
-- Global variables:
--	variable: x location: 1 value: 3
--	variable: y location: 2 value: 6
-- Number of cells used: 3
-- Memory dump: (1,3)(2,6)(3,4)

factorialWithLocal :: Program
factorialWithLocal =

     Program "FactorialWithLocal"

        (Dec "x" (N 3)   -- Global Variables
        (Dec "y" (N 1)
         EndDec))

        (Block

           (Dec "z" (N 1)   -- Local Variable
            EndDec)

           (While (Le (V "z" ) (V "x"))
               (Comp
                   (Ass "y" (Mult (V "y") (V "z")))
                   (Ass "z" (Add (V "z") (N 1))))))

-- Problema 2.b - Semántica natural modificada para eliminar Variables locales del store

-- El programa Factorial no se ve afectado porque no usa variables locales

-- |
-- >> showFinalState factorial
-- Program Factorial executed.
-- Global variables:
--	variable: x location: 1 value: 0
-- 	variable: y location: 2 value: 6
-- Number of cells used: 2
-- Memory dump: (1,0)(2,6)

-- El programa FactorialConLocal no deja variables locales en el store

-- |
-- >> showFinalState factorialWithLocal
-- Program FactorialWithLocal executed.
-- Global variables:
-- 	variable: x location: 1 value: 3
-- 	variable: y location: 2 value: 6
-- Number of cells used: 2
-- Memory dump: (1,3)(2,6)

-- Otro programa con muchas variables locales

-- |
-- >> showFinalState factorialWeird
-- Program FactorialWeird executed.
-- Global variables:
-- 	variable: x location: 1 value: 1
-- 	variable: y location: 2 value: 120
-- 	variable: z location: 3 value: 12
-- Number of cells used: 3
-- Memory dump: (1,1)(2,120)(3,12)

factorialWeird :: Program
factorialWeird =

     Program "FactorialWeird"

       (Dec "x" (N 5)   -- Global Variables
       (Dec "y" (N 1)
       (Dec "z" (N 0)
        EndDec)))

       (Block

          (Dec "lx" (N 500)   -- Local Variables
          (Dec "ly" (N 100)
          (Dec "lz"  let1
           EndDec)))

          (Comp (Ass "z" (V "lz"))
                (While (Neg (Eq (V "x") (N 1)))
                    (Comp (Ass "y" (Mult (V "y") (V "x")))
                          (Ass "x" (Sub (V "x") (N 1)))))))

-- Problema 3 - loop s1 exit on b s2

-- |
-- >> showFinalState loopFactorial
-- Program FactorialWithLoop executed.
-- Global variables:
-- 	variable: x location: 1 value: 6
-- 	variable: y location: 2 value: 720
-- Number of cells used: 2
-- Memory dump: (1,6)(2,720)

loopFactorial :: Program
loopFactorial =

     Program "FactorialWithLoop"

       (Dec "x" (N 6)   -- Global Variables
       (Dec "y" (N 1)
        EndDec))

       (Block

          (Dec "i" (N 1)   -- Local Variables
           EndDec)

          (Loop
             (Ass "y" (Mult (V "y") (V "i")))
             (Eq (V "i") (V "x"))
             (Ass "i" (Add (V "i") (N 1)))))

------------------------------------------------------------
-- code to execute examples and print the final state
------------------------------------------------------------

-- | 'showFinalState' executes the program 'p' and prints a report with the final state.

showFinalState :: Program -> IO ()
showFinalState p@(Program ident vars _) =
  putStr $ "Program " ++ ident ++ " executed.\n" ++
           "Global variables:\n" ++ showDecV vars envV sto ++
           "Number of cells used: " ++ show (sto next - 1) ++ "\n" ++
           "Memory dump: " ++ concatMap show (showStore sto) ++ "\n"
  where
    (envV, sto) = execProgram p

-- | 'showDecV' shows the variables declared in a 'DecVar', that is, the section
-- | of a block containing variable declarations. For each variable 'v' in the list
-- | 'vars', it shows its location and value:

showDecV :: DecVar -> EnvVar -> Store -> String
showDecV decs env sto = foldr (showVar env  sto) [] (varsOf decs)
      where
        showVar env sto x s =
          "\tvariable: " ++ x ++ " location: " ++ show (env x) ++ " value: " ++ show (sto . env $ x) ++ "\n" ++ s

        varsOf :: DecVar -> [Var]
        varsOf EndDec         = []
        varsOf (Dec v _ decs) = v : varsOf decs

-- | 'showStore' shows the contents of a 'Store' (a memory dump).
-- | Recall that variable names are missing, but you can relate
-- | memory cells to variables by numbering the variables from 1.

showStore :: Store -> [(Integer, Integer)]
showStore sto = [ (l, v) | l <- [1..sto next - 1], let v = sto l ]
