----------------------------------------------------------------------
--
-- While20.hs
-- Programming Languages
-- Fall 2020
--
-- Author: Pablo López
--- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Abstract syntax
-- -------------------------------------------------------------------

module While20  where

type Var = String

data Aexp = N Integer
          | V Var
          | Add Aexp Aexp
          | Mult Aexp Aexp
          | Sub Aexp Aexp
          | Let DecVar Aexp  -- | todo
          deriving Show

data Bexp = TRUE
          | FALSE
          | Eq Aexp Aexp
          | Le Aexp Aexp
          | Neg Bexp
          | And Bexp Bexp
          deriving Show

data DecVar = Dec Var Aexp DecVar
            | EndDec
            deriving Show

data  Stm = Ass Var Aexp
          | Skip
          | Comp Stm Stm
          | If Bexp Stm Stm
          | While Bexp Stm
          | Loop Stm Bexp Stm  -- | todo
          | Block DecVar Stm
          deriving Show

data Program = Program String DecVar Stm -- | todo
             deriving Show
