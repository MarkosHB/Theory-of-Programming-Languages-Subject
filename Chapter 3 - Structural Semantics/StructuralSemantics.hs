{-|

Programming Languages
Fall 2023

Implementation of the Structural Operational Semantics of the WHILE Language

Author: Marcos Hidalgo Baños

-}

module StructuralSemantics where

import           Aexp
import           Bexp
import           State
import           While

-- representation of configurations for While

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s
            | Stuck Stm State  -- <S, s>

data Update = Var :=>: Z

isFinal :: Config -> Bool
isFinal (Final _) = True
isFinal _         = False

isInter :: Config -> Bool
isInter (Inter _ _) = True
isInter _           = False

isStuck :: Config -> Bool
isStuck (Stuck _ _) = True
isStuck _           = False

update :: State -> Update -> State
update s (x :=>: v) y = if x==y then v else s y

-- representation of the transition relation <S, s> => gamma

sosStm :: Config -> Config

-- x := a

sosStm (Inter (Ass x a) s) = Final (update s (x :=>: (aVal a s)))

-- skip

sosStm (Inter (Skip) s) = Final s

-- s1; s2
-- [comp_sos1]
sosStm (Inter (Comp ss1 ss2) s) 
 | isInter next = Inter (Comp ss1' ss2) s'
    where
        next = sosStm (Inter ss1 s)
        Inter ss1' s' = next
-- [comp_sos2]
sosStm (Inter (Comp ss1 ss2) s)
 | isFinal next = Inter ss2 s'
    where
        next = sosStm (Inter ss1 s)
        Final s' = next

-- if b then s1 else s2
-- B[b]s = tt
sosStm (Inter (If b ss1 ss2) s) 
 | bVal b s = Inter ss1 s
-- B[b]s = ff
sosStm (Inter (If b ss1 ss2) s) 
 | not (bVal b s) = Inter ss2 s

-- while b do s

sosStm (Inter (While b ss) s) = Inter (If b (Comp ss (While b ss)) Skip) s

-- repeat s until b

sosStm (Inter (Repeat ss b) s) = Inter (Comp ss (If b Skip (Repeat ss b))) s

-- for x a1 to a2 s

sosStm (Inter (For x a1 a2 ss) s) = Inter (If (Leq a1 a2) (Comp (Ass x a1) (For x (Add v1 (N "1")) v2 ss)) Skip ) s
    where 
        v1 = N ( show (aVal a1 s))
        v2 = N ( show (aVal a2 s))

-- Do S While b

sosStm (Inter (DoWhile ss b) s) = Inter (Comp ss (If b (DoWhile ss b) Skip)) s

-- abort

sosStm (Inter stm s) = Stuck stm s
