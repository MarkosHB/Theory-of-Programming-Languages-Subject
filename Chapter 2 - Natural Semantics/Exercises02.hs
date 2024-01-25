{-|

Programming Languages
Fall 2023

Implementation of the Natural Semantics of the WHILE Language

Author: Marcos Hidalgo Baños

-}

module Exercises02 where

import           NaturalSemantics
import           Test.HUnit       hiding (State)

import           Aexp
import           Bexp
import           State
import           While
import           WhileExamples
import           WhileParser
import           Expressions

-- |----------------------------------------------------------------------
-- | Exercise 1
-- |----------------------------------------------------------------------
-- | The function 'sNs' returns the final state of the execution of a
-- | WHILE statement 'st' from a given initial state 's'. For example:

execFactorial :: State
execFactorial = sNs factorial factorialInit

-- | returns the final state:
-- |
-- |    s x = 1
-- |    s y = 6
-- |    s _ = 0
-- |
-- | Since a state is a function it cannot be printed thus you cannot
-- | add 'deriving Show' to the algebraic data type 'Config'.
-- | The goal of this exercise is to define a function to "show" a state
-- | thus you can inspect the final state yielded by the natural semantics
-- | of WHILE.

-- | Exercise 1.1
-- | Define a function 'showState' that given a state 's' and a list
-- | of variables 'vs' returns a list of strings showing the bindings
-- | of the variables mentioned in 'vs'. For example, for the state
-- | 's' above we get:
-- |
-- |    showState s ["x"] = ["x -> 1"]
-- |    showState s ["y"] = ["y -> 6"]
-- |    showState s ["x", "y"] = ["x -> 1", "y -> 6"]
-- |    showState s ["y", "z", "x"] = ["y -> 6", "z -> 0", "x -> 1"]

showState :: State -> [Var] -> [String]
showState s xs = map (\ x -> x ++ "->" ++ show (s x)) xs

-- | Test your function with HUnit.


-- | Using the function 'sNs' to execute a WHILE program is handy a bit awkward.
-- | The WHILE statement must be provided in abstract syntax and the initial
-- | state must be explicitly given and inspected.
-- |
-- | The 'run' function allows to execute a WHILE program stored in a file
-- | in concrete syntax and reports the final value of the variables mentioned
-- | in the program header. For example:
-- |
-- |    > run "Examples/Factorial.w"
-- |    Program Factorial finalized.
-- |    Final State: ["x->0","y->120"]

-- | Run the WHILE program stored in filename and show final values of variables
run :: FilePath -> IO()
run filename =
  do
     (programName, vars, stm) <- parser filename
     let Final s = nsStm (Inter stm (const 0))
     putStrLn $ "Program " ++ programName ++ " finalized."
     putStr "Final State: "
     print $ showState s vars

-- | Exercise 1.2
-- | Use the function 'run' to execute the WHILE programs 'Factorial.w' and 'Divide.w'
-- | in the directory 'Examples' to check your implementation of the Natural Semantics.
-- | Write a few more WHILE programs. For example, write a WHILE program
-- | "Power.w" to compute x^y.


-- |----------------------------------------------------------------------
-- | Exercise 2
-- |----------------------------------------------------------------------
-- | The WHILE language can be extended with a 'repeat S until b' statement.
-- | The file Examples/FactorialRepeat.w contains a simple program to
-- | compute the factorial with a 'repeat until' loop.

-- | Exercise 2.1
-- | Define the natural semantics of this new statement. You are not allowed
-- | to rely on the 'while b do S' statement.

{- Formal definition of 'repeat S until b'

[repeat_tt]

        < S, s > -> s' 
      -----------------------------------------
       < repeat S until b, s > -> s'
  
[repeat_ff]

        < S, s > -> s' , < repeat S until b, s' > -> s''
      ---------------------------------------------------
       < repeat S until b, s > -> s''

-}

-- | Exercise 2.2
-- | Extend the definition of 'nsStm' in module NaturalSemantics.hs
-- | to include the 'repeat S until b' statement.


-- | Exercise 2.3
-- | Write a couple of WHILE programs that use the 'repeat' statement and
-- | test your functions with HUnit and 'run'.


-- |----------------------------------------------------------------------
-- | Exercise 3
-- |----------------------------------------------------------------------
-- | The WHILE language can be extended with a 'for x:= a1 to a2 do S'
-- | statement.
-- | The file Examples/FactorialFor.w contains a simple program to compute
-- | the factorial with a 'for' loop.
-- | The file Examples/ForTests.w contains a more contrived example illustrating
-- | some subtle points of the semantics of the for loop.

-- | Exercise 3.1
-- | Define the natural semantics of this new statement. You are not allowed
-- | to rely on the 'while b do S' or the 'repeat S until b' statements.

{- Formal definition of 'for x:= a1 to a2 do S'

[for_ff] x >= a2

        ---------------------------------------------
          < for x:= a1 to a2 do S, s > --> s

[for_tt] x < a2

         < Ass x a1, s > --> s' , < S, s' > -> s'' , < for x:=a1+1 to a2 do S, s'' > -> s'''
        ------------------------------------------------------------------------------------
          < for x:= a1 to a2 do S, s > --> s'''
-}

-- | Exercise 3.2
-- | Extend  the definition 'nsStm' in module NaturalSemantics.hs
-- | to include the 'for x:= a1 to a2 do S' statement.

-- | Exercise 3.3
-- | Write a couple of WHILE programs that use the 'for' statement
-- | and test your functions with HUnit and 'run'.

{- Formal definition of 'do S while b'

[do_ff] 
           < S, s > --> s'
        ---------------------------------------------
          < do S while b, s > --> s'

[do_tt]

         < S, s > --> s' , < do S while b, s' > --> s'' 
        -------------------------------------------------
          < do S while b, s > --> s''
-}

-- |----------------------------------------------------------------------
-- | Exercise 4
-- |----------------------------------------------------------------------

-- | Define the semantics of arithmetic expressions (Bexp) by means of
-- | natural semantics. To that end, define an algebraic datatype 'ConfigAexp'
-- | to represent the configurations, and a function 'nsAexp' to represent
-- | the evaluation judgement.

-- representation of configurations for Aexp

data ConfigAExp = InterAexp Aexp State  -- <a, s>
                 | FinalAexp Z      -- Z

-- representation of the evaluation judgement <a, s> -> z

aexpStm :: ConfigAExp -> ConfigAExp
aexpStm exp = distinguir exp
  where
    distinguir (FinalAexp z) = FinalAexp z
    distinguir (InterAexp (N n) s) = FinalAexp (read n)
    distinguir (InterAexp (V x) s) = FinalAexp (s x)
    distinguir (InterAexp (Add a1 a2) s) = FinalAexp (a1' + a2')
      where
        FinalAexp a1' = aexpStm (InterAexp a1 s)
        FinalAexp a2' = aexpStm (InterAexp a2 s)
    distinguir (InterAexp (Mult a1 a2) s) = FinalAexp (a1' * a2')
      where
        FinalAexp a1' = aexpStm (InterAexp a1 s)
        FinalAexp a2' = aexpStm (InterAexp a2 s)
    distinguir (InterAexp (Sub a1 a2) s) = FinalAexp (a1' - a2')
      where
        FinalAexp a1' = aexpStm (InterAexp a1 s)
        FinalAexp a2' = aexpStm (InterAexp a2 s)

nsAexp :: Aexp -> State -> Z
nsAexp ss s = z
  where
    FinalAexp z = aexpStm (InterAexp ss s)

-- | Test your function with HUnit. Inspect the final values of at least
-- | four different evaluations.

testNsAexp :: Test
testNsAexp = TestList [
  nsAexp (N "1") factorialInit ~?= 1,
  nsAexp (V "y") factorialInit ~?= 0,
  nsAexp (Add (N "1") (N "2")) factorialInit ~?= 3,
  nsAexp (Mult (N "1") (N "2")) factorialInit ~?= 2,
  nsAexp (Sub (N "1") (N "2")) factorialInit ~?= (-1)
  ]

-- | Define the semantics of arithmetic expressions (Bexp) by means of
-- | natural semantics. To that end, define an algebraic datatype 'ConfigBexp'
-- | to represent the configurations, and a function 'nsBexp' to represent
-- | the evaluation judgement.

data ConfigBExp = InterBexp Bexp State  -- <b, s>
                 | FinalBexp Bool     -- b

bexpStm :: ConfigBExp -> ConfigBExp
bexpStm exp = distinguir exp
  where
    distinguir (FinalBexp b) = FinalBexp b
    distinguir (InterBexp TRUE s) = FinalBexp True
    distinguir (InterBexp FALSE s) = FinalBexp False
    distinguir (InterBexp (Equ a1 a2) s) = FinalBexp (a1' == a2')
      where
        FinalAexp a1' = aexpStm (InterAexp a1 s)
        FinalAexp a2' = aexpStm (InterAexp a2 s)
    distinguir (InterBexp (Leq a1 a2) s) = FinalBexp (a1' <= a2')
      where
        FinalAexp a1' = aexpStm (InterAexp a1 s)
        FinalAexp a2' = aexpStm (InterAexp a2 s)
    distinguir (InterBexp (Neg b) s) = FinalBexp (not b')
      where
        FinalBexp b' = bexpStm (InterBexp b s)
    distinguir (InterBexp (And b1 b2) s) = FinalBexp (b1' && b2')
      where
        FinalBexp b1' = bexpStm (InterBexp b1 s)
        FinalBexp b2' = bexpStm (InterBexp b2 s)
 
nsBexp :: Bexp -> State -> Bool
nsBexp b s = nb
  where
    FinalBexp nb = bexpStm (InterBexp b s)

-- | Test your function with HUnit. Inspect the final values of at least
-- | four different evaluations.

testNsBexp :: Test
testNsBexp = TestList [
  nsBexp (Leq (N "1") (N "5")) factorialInit ~?= True,
  nsBexp (Equ (N "1") (N"1")) factorialInit ~?= True,
  nsBexp (Neg FALSE) factorialInit ~?= True,
  nsBexp (And (Leq (N "1") (N "5")) (Equ (N "1") (N"1"))) factorialInit ~?= True
  ]

-- |----------------------------------------------------------------------
-- | Exercise 5
-- |----------------------------------------------------------------------

-- | In the statement 'for x:= a1 to a2 S' the variable 'x' is the control
-- | variable. Some programming languages protect this variable in that
-- | it cannot be assigned to in the body of the loop, S.
-- |
-- | For example, the program below:
-- |
-- |    y := 1;
-- |    for x:= 1 to 10 do begin
-- |       y := y * x;
-- |       x := x + 1    // assignment to control variable
-- |    end
-- |
-- | would be rejected by languages enforcing such a restriction.
-- | Note that this check is performed before the program is executed,
-- | and therefore is a static semantics check.

-- | Exercise 5.1
-- | Define the static semantics by means of a judgement that is valid
-- | if and only if the program does not overwrite its control variables.
-- | Use axioms and inference rules to validate your judgements.

-- | Exercise 5.2
-- | Define a function 'forLoopVariableCheck' that implements the static
-- | semantics check above described. Use the function 'analyze'
-- | to check your implementation.

fvStm :: Stm -> [Var]
fvStm (Ass x a) = norepe (x : fvAexp a)
fvStm (Skip) = []
fvStm (Comp ss1 ss2) = norepe ((fvStm ss1) ++ (fvStm ss2))
fvStm (If b ss1 ss2) = norepe((fvBexp b) ++ (fvStm ss1) ++ (fvStm ss2))
fvStm (While b ss) = norepe((fvBexp b) ++ (fvStm ss))
fvStm (Repeat ss b) = norepe((fvStm ss) ++ (fvBexp b))
fvStm (For x a1 a2 ss) = norepe((fvAexp a1) ++ (fvAexp a2) ++ (fvStm ss))


testfvStm :: Test
testfvStm = TestList [ "Test1" ~: [] ~=? fvStm (Skip),
                        "Test2" ~: ["x"] ~=? fvStm (Ass "x" (N "1")),
                        "TestPower" ~: ["z", "x", "y"] ~=? fvStm(Comp (Ass "z" (N "1"))
                                                                  (While (Neg (Equ (V "y") (N "0")))
                                                                      (Comp (Ass "z" (Mult (V "z") (V "x")))
                                                                            (Ass "y" (Sub (V "y") (N "1"))))))
                      ]


forLoopVariableCheck :: Stm -> Bool
forLoppVariableCheck (For x a1 a2 ss) = not (elem x (fvStm ss)) && forLoopVariableCheck ss -- Si x no es una variable del cuerpo del bucle
forLoopVariableCheck (Repeat ss b) = forLoopVariableCheck ss
forLoopVariableCheck (While b ss) = forLoopVariableCheck ss
forLoopVariableCheck (If b ss1 ss2) = (forLoopVariableCheck ss1) && (forLoopVariableCheck ss2)
forLoopVariableCheck (Comp ss1 ss2) = (forLoopVariableCheck ss1) && (forLoopVariableCheck ss2)
forLoopVariableCheck _ = True -- Ass, Skip

-- | Analyze the WHILE program stored in filename and show results
analyze :: FilePath -> IO()
analyze filename =
  do
     (program, _, stm) <- parser filename
     putStrLn $ "Analyzing program " ++ program
     let ok = forLoopVariableCheck stm
     if ok then putStrLn "Program accepted"
     else putStrLn "Program rejected"

-- | Convert concrete syntax to abstract syntax
concreteToAbstract :: FilePath -> FilePath -> IO()
concreteToAbstract inputFile outputFile =
  do
    (_, _, stm) <- parser inputFile
    let s = show stm              -- | have 'show' replaced by a pretty printer
    if null outputFile
      then putStrLn s
      else writeFile outputFile s
