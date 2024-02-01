-- -------------------------------------------------------------------
--
-- Aexp.hs
--
-- Abstract syntax and semantics of Aexp
-- [Nielson and Nielson, Semantics with Applications]
--
-- -------------------------------------------------------------------

module Aexp where

-- abstract syntax of Aexp

type Var = String

data Aexp = N Integer
          | V Var
          | Add Aexp Aexp
          | Mult Aexp Aexp
          | Sub Aexp Aexp
          deriving (Show, Eq)

-- semantic domains

type  Z     = Integer
type  T     = Bool
type  State = Var -> Z

-- semantic function A[a]s

aVal :: Aexp -> State -> Z
aVal (N n) _        =  n
aVal (V x) s        =  s x
aVal (Add a1 a2) s  =  aVal a1 s + aVal a2 s
aVal (Mult a1 a2) s =  aVal a1 s * aVal a2 s
aVal (Sub a1 a2) s  =  aVal a1 s - aVal a2 s
