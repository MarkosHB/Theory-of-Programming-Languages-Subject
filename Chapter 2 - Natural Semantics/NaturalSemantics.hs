{-|

Programming Languages
Fall 2023

Implementation of the Natural Semantics of the WHILE Language

Author:

-}

module NaturalSemantics where

import           Aexp
import           Bexp
import           State
import           While

-- representation of configurations for WHILE

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s

data Update = Var :=>: Z

-- | define a function 'update' that takes a state 's' and an update 'x :=> v'
-- | and returns the updated state 's [x :=> v]'

update :: State -> Update -> State
update s (x :=>: v) y = if x==y then v else s y

-- representation of the execution judgement <S, s> -> s'

nsStm :: Config -> Config

-- x := a

nsStm (Inter (Ass x a) s)      = Final (update s (x :=>: aVal a s))

-- skip

nsStm (Inter Skip s)           = Final s

-- s1; s2

nsStm (Inter (Comp ss1 ss2) s) = Final s''
  where
    Final s' = nsStm (Inter ss1 s)
    Final s'' = nsStm (Inter ss2 s')

-- if b then s1 else s2

-- B[b]s = tt
nsStm (Inter (If b ss1 ss2) s) 
  | (bVal b s) = nsStm (Inter ss1 s)

-- B[b]s = ff
nsStm (Inter (If b ss1 ss2) s)
  | not (bVal b s) = nsStm (Inter ss2 s)

-- while b do s

-- B[b]s = ff
nsStm (Inter (While b ss) s)
  | not (bVal b s) = Final s

-- B[b]s = tt
nsStm (Inter (While b ss) s)
  | (bVal b s) = Final s''
    where
      Final s' = nsStm (Inter ss s)
      Final s'' = nsStm (Inter (While b ss) s')

-- repeat S until b
-- B[b]s == ff
nsStm (Inter (Repeat ss b))
  | (bVal b s) = nsStm (Inter ss s)

-- B[b]s == tt
nsStm (Inter (Repeat ss b))
  | not (bVal b s) = Final s''
  where
    Final s' = nsStm (Inter ss s)
    Final s'' = nsStm (Inter (Repeat ss b) s')

-- for x:= a1 to a2 do S
-- B[a1 > a2] s = ff
nsStm (Inter (For x a1 a2 ss) s)
  | not (bVal (Leq a1 a2) s) = Final s

-- B[a1 <= a2] s = tt
nsStm (Inter (For x a1 a2 ss) s)
  | bVal (Leq a1 a2) s = Final s'''
  where
    Final s' = nsStm (Inter (Ass x a1) s)
    Final s'' = nsStm (Inter ss s')
    Final s''' = nsStm (Inter (For x (Add v1 (N "1")) v2 ss) s'')
      v1 = N (show (aVal a1 s0))
      v2 = N (show (aVal a2 s0))

-- semantic function for natural semantics
sNs :: Stm -> State -> State
sNs ss s = s'
  where Final s' = nsStm (Inter ss s)

