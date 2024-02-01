{-|

Programming Languages
Enero 2022

Abstract syntax of While with Blocks and Dynamic Scope

Author: Pablo López

-}

-- -------------------------------------------------------------------
-- Abstract syntax
-- -------------------------------------------------------------------

module While where

import           Aexp
import           Bexp

-- abstract syntax of While

data Stm = Ass Var Aexp
         | Skip
         | Comp Stm Stm
         | Block DecVar Stm      -- | todo
         | Select SelectCases    -- | todo
         | For Stm Bexp Stm Stm  -- | todo
         deriving Show

data DecVar = Dec Var Aexp DecVar
            | EndDec
            deriving Show

data SelectCases = Case Bexp Stm SelectCases
                 | EndCase Bexp Stm
                 deriving Show
