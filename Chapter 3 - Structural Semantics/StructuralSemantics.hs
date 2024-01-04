{-|

Programming Languages
Fall 2023

Implementation of the Structural Operational Semantics of the WHILE Language

Author:

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
sosStm (Inter (Ass x a) s) = Final (update s (x :=>: aVal a s))

-- skip
sosStm (Inter Skip s) = Final s

-- s1; s2
sosStm (Inter (Comp s1 s2) s)  --comp1
    | isInter next = Inter (Comp s1' s2) s'
    where
        next = sosStm (Inter s1 s)
        Inter s1' s' = next

sosStm (Inter (Comp s1 s2) s)  --comp2
    | isFinal next = Inter s2 s'
    where
        next = sosStm (Inter s1 s)
        Final s' = next
 
-- if b then s1 else s2
sosStm (Inter (If expr s1 s2) s)
    | (bVal expr s) = Inter s1 s
sosStm (Inter (If expr s1 s2) s)
    | not (bVal expr s) = Inter s2 s 

-- while b do s
sosStm (Inter w@(While b s1) s) = Inter (If b (Comp s1 w) Skip) s

-- repeat s until b
-- B[b]s == ff
sosStm (Inter (Repeat ss b) s)
    | not (bVal b s) = sosStm (Inter ss s)
-- B[b]s == tt
sosStm (Inter (Repeat ss b) s)
    | (bVal b s) = Final s

-- for x a1 to a2 s
--a1 < a2
sosStm (Inter (For v a1 a2 stm) s)
    |   aVal (V v) s < aVal a2 s = Inter (Comp stm (For v a1 a2 stm)) s'
        where
            s' = update s (v :=>: (aVal (V v) s + 1))
            
--a1 == a2
sosStm (Inter (For v a1 a2 stm) s)
    |   aVal (V v) s == aVal a2 s = Inter stm s

--a1 > a2
sosStm (Inter (For v a1 a2 stm) s)
    |   aVal (V v) s > aVal a2 s = Final s


-- abort
sosStm (Inter stm s) = Stuck stm s

