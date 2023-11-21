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
import           Data.List

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

bitVal :: Bit -> Integer
bitVal O = 0
bitVal I = 1

binVal :: Bin -> Z
binVal (MSB b) = bitVal b
binVal (B xs b) = binVal xs * 2 + bitVal b

-- | Test your function with HUnit.

testBinVal :: Test
testBinVal = test ["value of zero"  ~: 0 ~=? binVal zero,
                   "value of one"   ~: 1 ~=? binVal one,
                   "value of three" ~: 3 ~=? binVal three,
                   "value of six"   ~: 6 ~=? binVal six]

-- | Define a function 'foldBin' to fold a value of type 'Bin'
-- MSB :: Bit -> Bin        => fcb
-- B :: Bin -> Bit -> Bin   => fr
-- Reemplazar cada constructor por una funcion que devuelve 'a'
foldBin :: (a -> Bit -> a) -> (Bit -> a) -> Bin -> a
foldBin fr fcb bin = plegar bin
    where
        plegar (MSB bit) = fcb bit
        plegar (B bin bit) = fr (plegar bin) bit

-- | and use 'foldBin' to define a function 'binVal''  equivalent to 'binVal'.

binVal' :: Bin -> Integer
binVal' = foldBin (\ z bit -> 2 * z + bitVal bit) bitVal

-- | Test your function with HUnit.
testbinVal' :: Test
testbinVal' = test ["value of zero"  ~: 0 ~=? binVal' zero,
                   "value of one"   ~: 1 ~=? binVal' one,
                   "value of three" ~: 3 ~=? binVal' three,
                   "value of six"   ~: 6 ~=? binVal' six]

-- | Define a function 'normalize' that given a binary numeral trims leading zeroes.
normalizeBit :: Bin -> Bit -> Bin
normalizeBit (MSB O) b = MSB b 
normalizeBit bin b = B bin b

normalize :: Bin -> Bin
normalize (MSB b) = MSB b
normalize (B bin bit) = normalizeBit (normalize bin) bit

-- | and use 'foldBin' to define a function 'normalize''  equivalent to 'normalize'.

normalize' :: Bin -> Bin
normalize' = foldBin normalizeBit MSB

-- | Test your functions with HUnit.
testnormalize' :: Test
testnormalize' = test ["primera prueba"  ~: B(B(MSB I) O) I ~=? normalize (B (B (B (B (MSB O) O) I) O) I),
                   "segunda prueba"  ~: MSB O ~=? normalize (B (B (B (B (MSB O) O) O) O) O),
                   "tercera prueba" ~: B(B(MSB I) O) I ~=? normalize' (B (B (B (B (MSB O) O) I) O) I),
                   "cuarta prueba"  ~: MSB O ~=? normalize' (B (B (B (B (MSB O) O) O) O) O)]

-- EXTRA.
testBinValue :: (Bin -> Z) -> Test
testBinValue f = test ["value of zero"  ~: 0 ~=? f zero,
                   "value of one"   ~: 1 ~=? f one,
                   "value of three" ~: 3 ~=? f three,
                   "value of six"   ~: 6 ~=? f six]

foldList :: (a->b->b) ->b -> [a] -> b
foldList f base xs = plegar xs 
    where
        plegar [] = base    -- [] :: [a] ---> base :: b 
        plegar (x:xs) =  f x (plegar xs)   
            -- (:) :: a -> [a] -> [a] ---> f :: a -> b -> b

-- |----------------------------------------------------------------------
-- | Exercise 2 - Free variables of expressions
-- |----------------------------------------------------------------------
-- | Define the function 'fvAexp' that computes the set of free variables
-- | occurring in an arithmetic expression. Ensure that each free variable
-- | occurs once in the resulting list.

fvAexp :: Aexp -> [Var]
fvAexp expr = distinguir expr
    where 
        distinguir (N n) = []
        distinguir (V x) = [x]
        distinguir (Add a1 a2) = norepe (distinguir a1 ++ distinguir a2)
        distinguir (Mult a1 a2) = norepe (distinguir a1 ++ distinguir a2)
        distinguir (Sub a1 a2) = norepe (distinguir a1 ++ distinguir a2)

norepe :: [Var] -> [Var]
norepe [] = []
norepe (x:xs) = if elem x xs then (norepe xs) else x:(norepe xs)

-- | Test your function with HUnit.
testFvAexp :: Test
testFvAexp = test ["FV: " ~: [] ~=? fvAexp(N "1"),
                   "FV: " ~: ["x"] ~=? fvAexp(V "x"),
                   "FV: " ~: ["x"] ~=? fvAexp(Add (V "x") (V "x")),
                   "FV: " ~: ["x", "y"] ~=? fvAexp(Mult (V "y") (V "p"))]


-- | Define the function 'fvBexp' that computes the set of free variables
-- | occurring in a Boolean expression.

fvBexp :: Bexp -> [Var]
fvBexp expr = distinguir expr
    where 
        distinguir (TRUE) = []
        distinguir (FALSE) = []
        distinguir (Equ a1 a2) = norepe (fvAexp a1 ++ fvAexp a2)
        distinguir (Leq a1 a2) = norepe (fvAexp a1 ++ fvAexp a2)
        distinguir (Neg a1) = norepe (distinguir a1)
        distinguir (And a1 a2) = norepe (distinguir a1 ++ distinguir a2)

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

-- | Test your function with HUnit.
--testSubstAexp :: Test
--testSubstAexp = test ["Test1: y" ~: V "y" ~=? substAexp (V "x") ("x" :-> V "y")]

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


-- |----------------------------------------------------------------------
-- | Exercise 4 - Update of state
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Update' for state updates:

data Update = Var :=>: Z

-- | define a function 'update' that takes a state 's' and an update 'x :=> v'
-- | and returns the updated state 's [x :=> v]'

update :: State -> Update -> State
update s (x :=>: v) y = if x==y then v else s y

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

-- |----------------------------------------------------------------------
-- | Exercise 5 - Folding expressions
-- |----------------------------------------------------------------------
-- | Define a function 'foldAexp' to fold an arithmetic expression

foldAexp :: (NumLit->a) -> (Var->a) -> (a->a->a) -> (a->a->a) -> (a->a->a) -> Aexp -> a
foldAexp fNum fVar fSum fMul fRes expr = plegar expr
    where
        plegar (N num) = fNum num
        plegar (V var) = fVar var
        plegar (Add a1 a2) = fSum (plegar a1) (plegar a2)
        plegar (Mult a1 a2) = fMul (plegar a1) (plegar a2)
        plegar (Sub a1 a2) = fRes (plegar a1) (plegar a2)

-- | Use 'foldAexp' to define the functions 'aVal'', 'fvAexp'', and 'substAexp''.

aVal' :: Aexp -> State -> Z
aVal' expr s = foldAexp read s (+) (*) (-) expr

fvAexp' :: Aexp -> [Var]
fvAexp' expr = foldAexp (\ n -> []) (\ n -> [n]) (++) (++) (++) expr

substAexp' :: Aexp -> Subst -> Aexp
substAexp' expr (y:->: a0) = foldAexp N (\ v -> if v == y then a0 else V v) Add Mult Sub expr

-- | Define a function 'foldBexp' to fold a Boolean expression and use it
-- | to define the functions 'bVal'', 'fvBexp'', and 'substAexp''.

foldBexp :: a -> a -> (Aexp->Aexp->a) -> (Aexp->Aexp->a) -> (a->a) -> (a->a->a) -> Bexp -> a
foldBexp fcbT fcbF fEqu fLeq fNeg fAnd expr = plegar expr
    where
        plegar (TRUE) = fcbT
        plegar (FALSE) = fcbF
        plegar (Equ a1 a2) = fEqu a1 a2
        plegar (Leq a1 a2) = fLeq a1 a2
        plegar (Neg b1) = fNeg (plegar b1)
        plegar (And b1 b2) = fAnd (plegar b1) (plegar b2)

bVal' :: Bexp -> State -> Bool
bVal' expr s = foldBexp True False fIqual fLessEqu not (&&) expr
    where
        fIqual = (\ a1 a2 -> (aVal' a1 s) == (aVal' a2 s))
        fLessEqu = (\ a1 a2 -> (aVal' a1 s) <= (aVal' a2 s))

fvBexp' :: Bexp -> [Var]
fvBexp' = undefined

substBexp' :: Bexp -> Subst -> Bexp
substBexp' b subst = foldBexp TRUE FALSE feq fle Neg And b
    where
        feq a1 a2 = Equ (substAexp' a1 subst) (substAexp' a2 subst)
        fle a1 a2 = Leq (substAexp' a1 subst) (substAexp' a2 subst)
