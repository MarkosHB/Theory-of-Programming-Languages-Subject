{-

Programming Languages
Fall 2023

Semantics of Expressions

-}

module Expressions where

import           Aexp
import           Bexp
import           State


import           Test.HUnit hiding (State)

-- |----------------------------------------------------------------------
-- | Exercise 1 - Semantics of binary numerals
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Bin' for the binary numerals:

data Bit = O
         | I
         deriving (Eq, Show)

data Bin = MSB Bit
         | B Bin Bit
         deriving (Eq, Show)

-- | and the following values of type 'Bin':

zero :: Bin
zero = MSB O

one :: Bin
one = MSB I

three :: Bin
three = B (B (MSB O) I) I

six :: Bin
six = B (B (MSB I) I) O

-- | define a semantic function 'binVal' that associates
-- | a number (in the decimal system) to each binary numeral.

bitVal :: Bit -> Z
bitVal O = 0
bitVal I = 1

binVal :: Bin -> Z
binVal (MSB b) = bitVal b
binVal (B bin bit) = binVal bin * 2 + bitVal bit

-- | Test your function with HUnit.

testBinVal :: Test
testBinVal = test ["value of zero"  ~: 0 ~=? binVal zero,
                   "value of one"   ~: 1 ~=? binVal one,
                   "value of three" ~: 3 ~=? binVal three,
                   "value of six"   ~: 6 ~=? binVal six]

-- | Define a function 'foldBin' to fold a value of type 'Bin'

foldBin :: (a-> Bit -> a) -> (Bit -> a) -> Bin -> a
foldBin fBin fBit bin = plegar bin
    where
        plegar (MSB bit) = fBit bit
        plegar (B bin bit) = fBin (plegar bin) bit 

-- | and use 'foldBin' to define a function 'binVal''  equivalent to 'binVal'.

binVal' :: Bin -> Integer
binVal' bin = foldBin (\ z bit -> 2*z+bitVal bit) bitVal bin

-- | Test your function with HUnit.

testBinVal' :: Test
testBinVal' = test ["value of zero"  ~: 0 ~=? binVal' zero,
                   "value of one"   ~: 1 ~=? binVal' one,
                   "value of three" ~: 3 ~=? binVal' three,
                   "value of six"   ~: 6 ~=? binVal' six]

-- | Define a function 'normalize' that given a binary numeral trims leading zeroes.

normalize :: Bin -> Bin
normalize = undefined

-- | and use 'foldBin' to define a function 'normalize''  equivalent to 'normalize'.

normalize' :: Bin -> Bin
normalize' = undefined

-- | Test your functions with HUnit.

-- todo

-- |----------------------------------------------------------------------
-- | Exercise 2 - Free variables of expressions
-- |----------------------------------------------------------------------
-- | Define the function 'fvAexp' that computes the set of free variables
-- | occurring in an arithmetic expression. Ensure that each free variable
-- | occurs once in the resulting list.

norepe :: [Var] -> [Var]
norepe [] = []
norepe (x:xs) = if elem x xs then (norepe xs) else x:(norepe xs)

fvAexp :: Aexp -> [Var]
fvAexp aexp = distinguir aexp
    where
        distinguir (N numLit) = []
        distinguir (V var) = [var]
        distinguir (Add a1 a2) = norepe (distinguir a1 ++ distinguir a2)
        distinguir (Mult a1 a2) = norepe (distinguir a1 ++ distinguir a2)
        distinguir (Sub a1 a2) = norepe (distinguir a1 ++ distinguir a2)
        distinguir (Div a1 a2) = norepe (distinguir a1 ++ distinguir a2)

-- | Test your function with HUnit.

testFvAexp :: Test
testFvAexp = test ["FV: " ~: [] ~=? fvAexp(N "1"),
                   "FV: " ~: ["x"] ~=? fvAexp(V "x"),
                   "FV: " ~: ["x", "y"] ~=? fvAexp(Add (V "x") (V "y")),
                   "FV: " ~: ["x"] ~=? fvAexp(Div (V "x") (N "0"))]

-- | Define the function 'fvBexp' that computes the set of free variables
-- | occurring in a Boolean expression.

fvBexp :: Bexp -> [Var]
fvBexp bexp = distinguir bexp
    where
        distinguir TRUE = []
        distinguir FALSE = []
        distinguir (Equ a1 a2) = norepe (fvAexp a1 ++ fvAexp a2)
        distinguir (Leq a1 a2) = norepe (fvAexp a1 ++ fvAexp a2)
        distinguir (Neg b1) = norepe (distinguir b1)
        distinguir (And b1 b2) = norepe (distinguir b1 ++ distinguir b2)

-- | Test your function with HUnit.

testFvBexp :: Test
testFvBexp = test ["FV: " ~: [] ~=? fvBexp(TRUE),
                   "FV: " ~: ["x"] ~=? fvBexp(Equ (V "x") (V "x")),
                   "FV: " ~: ["x"] ~=? fvBexp(Neg (Leq (V "x") (V "x"))),
                   "FV: " ~: ["x", "y"] ~=? fvBexp(And (Neg (Equ (V "y") (V "p"))) FALSE)]

-- |----------------------------------------------------------------------
-- | Exercise 3 - Substitution of variables in expressions
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Subst' for representing substitutions:

data Subst = Var :->: Aexp

-- | define a function 'substAexp' that takes an arithmetic expression
-- | 'a' and a substitution 'y:->:a0' and returns the substitution a [y:->:a0];
-- | i.e., replaces every occurrence of 'y' in 'a' by 'a0'.

substAexp :: Aexp -> Subst -> Aexp
substAexp expr subst = distinguir expr subst
    where
        distinguir (N n) _ = N n
        distinguir (V x) (y :->: a0) = if y == x then a0 else V x
        distinguir (Add a1 a2) subst = Add (distinguir a1 subst) (distinguir a2 subst)
        distinguir (Mult a1 a2) subst = Mult (distinguir a1 subst) (distinguir a2 subst)
        distinguir (Sub a1 a2) subst = Sub (distinguir a1 subst) (distinguir a2 subst)
        distinguir (Div a1 a2) subst = Div (distinguir a1 subst) (distinguir a2 subst)


-- | Test your function with HUnit.

testSubstAexp :: Test
testSubstAexp = test ["Test1: y" ~: V "y" ~=? substAexp (V "x") ("x" :->: V "y"),
                        "Test 2 : " ~: Div (V "y") (N "1") ~=? substAexp (Div (V "x") (N "1")) ("x" :->: V "y")] 


-- | Define a function 'substBexp' that implements substitution for
-- | Boolean expressions.

substBexp :: Bexp -> Subst -> Bexp
substBexp expr subst = distinguir expr subst
    where
        distinguir (TRUE) _ = TRUE
        distinguir (FALSE) _ = FALSE
        distinguir (Equ a1 a2) subst = Equ (substAexp a1 subst) (substAexp a2 subst)
        distinguir (Leq a1 a2) subst = Leq (substAexp a1 subst) (substAexp a2 subst)
        distinguir (Neg b1) subst = Neg (distinguir b1 subst)
        distinguir (And b1 b2) subst = And (distinguir b1 subst) (distinguir b2 subst)

-- | Test your function with HUnit.

-- todo

-- |----------------------------------------------------------------------
-- | Exercise 4 - Update of state
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Update' for state updates:

data Update = Var :=>: Z

-- | define a function 'update' that takes a state 's' and an update 'x :=> v'
-- | and returns the updated state 's [x :=> v]'

update :: State -> Update -> State
update s (x :=>: v) y = if x==y then v else s y

-- | Test your function with HUnit.

-- todo

-- | Define a function 'updates' that takes a state 's' and a list of updates
-- | 'us' and returns the updated states resulting from applying the updates
-- | in 'us' from head to tail. For example:
-- |
-- |    updates s ["x" :=>: 1, "y" :=>: 2, "x" :=>: 3]
-- |
-- | returns a state that binds "x" to 3 (the most recent update for "x").

updates :: State ->  [Update] -> State
updates s [] = s
updates s (x:xs) = updates (update s x) xs 

-- | Test your function with HUnit.

-- todo

-- |----------------------------------------------------------------------
-- | Exercise 5 - Folding expressions
-- |----------------------------------------------------------------------
-- | Define a function 'foldAexp' to fold an arithmetic expression

foldAexp :: (NumLit -> a) -> (Var -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> Aexp -> a
foldAexp fN fV fAdd fMult fSub fDiv a = distinguir a
    where
        distinguir (N n) = fN n
        distinguir (V v) = fV v
        distinguir (Add a1 a2) = fAdd (distinguir a1) (distinguir a2)
        distinguir (Mult a1 a2) = fMult (distinguir a1) (distinguir a2)
        distinguir (Sub a1 a2) = fSub (distinguir a1) (distinguir a2)
        distinguir (Div a1 a2) = fDiv (distinguir a1) (distinguir a2)


-- | Use 'foldAexp' to define the functions 'aVal'', 'fvAexp'', and 'substAexp''.

aVal' :: Aexp -> State -> Z
aVal' a s = foldAexp numLit s (+) (*) (-) div a

fvAexp' :: Aexp -> [Var]
fvAexp' a = foldAexp (\ n -> []) (\ n -> [n]) (++) (++) (++) (++) a

substAexp' :: Aexp -> Subst -> Aexp
substAexp' expr (y:->: a0) = foldAexp N (\ v -> if v == y then a0 else V v) Add Mult Sub Div expr

-- | Test your functions with HUnit.

-- todo

-- | Define a function 'foldBexp' to fold a Boolean expression and use it
-- | to define the functions 'bVal'', 'fvBexp'', and 'substAexp''.

foldBexp :: a -> a -> (Aexp -> Aexp -> a) -> (Aexp -> Aexp -> a) -> (a -> a) -> (a -> a -> a) -> Bexp -> a
foldBexp fTrue fFalse fEqu fLeq fNeg fAnd b = plegar b
    where
        plegar (TRUE) = fTrue
        plegar (FALSE) = fFalse
        plegar (Equ a1 a2) = fEqu a1 a2
        plegar (Leq a1 a2) = fLeq a1 a2
        plegar (Neg b) = fNeg (plegar b)
        plegar (And b1 b2) = fAnd (plegar b1) (plegar b2)

bVal' :: Bexp -> State -> Bool
bVal' b s = foldBexp True False fIqual fLessEqu not (&&) b
    where
        fIqual = (\ a1 a2 -> (aVal' a1 s) == (aVal' a2 s))
        fLessEqu = (\ a1 a2 -> (aVal' a1 s) <= (aVal' a2 s))

fvBexp' :: Bexp -> [Var]
fvBexp' b = foldBexp [] [] fEq fLeq fNeg (++) b
    where
        fNeg b = b
        fEq a1 a2 = norepe(fvAexp' a1 ++ fvAexp' a2)
        fLeq a1 a2 = norepe(fvAexp' a1 ++ fvAexp' a2)

substBexp' :: Bexp -> Subst -> Bexp
substBexp' b subst = foldBexp TRUE FALSE feq fle Neg And b
    where
        feq a1 a2 = Equ (substAexp' a1 subst) (substAexp' a2 subst)
        fle a1 a2 = Leq (substAexp' a1 subst) (substAexp' a2 subst)

-- | Test your functions with HUnit.
testFvBexp' :: Test
testFvBexp' = test ["FV: " ~: [] ~=? fvBexp'(TRUE),
                   "FV: " ~: ["x"] ~=? fvBexp'(Equ (V "x") (V "x")),
                   "FV: " ~: ["x"] ~=? fvBexp'(Neg (Leq (V "x") (V "x"))),
                   "FV: " ~: ["x", "y"] ~=? fvBexp'(And (Neg (Equ (V "x") (V "y"))) FALSE)]

