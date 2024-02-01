-- -------------------------------------------------------------------
--
-- Bexp.hs
--
-- Abstract syntax and semantics of Bexp
-- [Nielson and Nielson, Semantics with Applications]
--
-- -------------------------------------------------------------------

module Bexp where

import           Aexp

-- abstract syntax of Bexp

data Bexp = TRUE
          | FALSE
          | Eq Aexp Aexp
          | Le Aexp Aexp
          | Neg Bexp
          | And Bexp Bexp
          deriving (Show, Eq)

-- semantic function B[b]s

bVal :: Bexp -> State -> T
bVal TRUE _        =  True
bVal FALSE _       =  False
bVal (Eq a1 a2) s  =  aVal a1 s == aVal a2 s
bVal (Le a1 a2) s  =  aVal a1 s <= aVal a2 s
bVal (Neg b) s     =  not(bVal b s)
bVal (And b1 b2) s =  bVal b1 s && bVal b2 s
