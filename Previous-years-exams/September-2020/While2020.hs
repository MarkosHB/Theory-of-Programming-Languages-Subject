-- -------------------------------------------------------------------
--
-- While2020.hs
--
-- The language WHILE extended with swap and for.
--
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Secion B.1  Abstract syntax
-- -------------------------------------------------------------------

module While2020 where

type Var   =  String

data Aexp  =  N Integer
           |  V Var
           |  Add Aexp Aexp
           |  Mult Aexp Aexp
           |  Sub Aexp Aexp
           deriving (Show, Eq)

data Bexp  =  TRUE
           |  FALSE
           |  Eq Aexp Aexp
           |  Le Aexp Aexp
           |  Neg Bexp
           |  And Bexp Bexp
           deriving (Show, Eq)

-- | added swap and for statements

data Stm   =  Ass Var Aexp
           |  Skip
           |  Comp Stm Stm
           |  If Bexp Stm Stm
           |  While Bexp Stm
           |  Swap Var Var          -- todo
           |  For Stm Bexp Stm Stm  -- todo
           deriving Show

-- Example B.1

factorial :: Stm
factorial = Comp (Ass "y" (N 1))
                 (While (Neg (Eq (V "x") (N 1)))
                    (Comp (Ass "y" (Mult (V "y") (V "x")))
                          (Ass "x" (Sub (V "x") (N 1)))))

-- End Example B.1

---------------------------------------------------------------------
-- Secion B.2  Evaluation of Boolean and arithmetic expressions
---------------------------------------------------------------------

type Z      =  Integer
type T      =  Bool
type State  =  Var -> Z

-- Example B.3

sInit :: State
sInit "x" =  3
sInit "y" =  5
sInit  _  =  0

-- End Example B.3

aVal :: Aexp -> State -> Z
aVal (N n) _        =  n
aVal (V x) s        =  s x
aVal (Add a1 a2) s  =  aVal a1 s + aVal a2 s
aVal (Mult a1 a2) s =  aVal a1 s * aVal a2 s
aVal (Sub a1 a2) s  =  aVal a1 s - aVal a2 s

bVal :: Bexp -> State -> T
bVal TRUE _        =  True
bVal FALSE _       =  False
bVal (Eq a1 a2) s  =  aVal a1 s == aVal a2 s  -- equivalent but smaller
bVal (Le a1 a2) s  =  aVal a1 s <= aVal a2 s  -- equivalent but smaller
bVal (Neg b) s     =  not(bVal b s)           -- equivalent but smaller
bVal (And b1 b2) s =  bVal b1 s && bVal b2 s  -- equivalent but smaller
