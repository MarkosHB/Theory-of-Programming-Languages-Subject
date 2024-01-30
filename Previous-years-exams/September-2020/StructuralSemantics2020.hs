-- -------------------------------------------------------------------
-- Structural Semantics for WHILE 2020.
-- Examen de Lenguajes de Programación. UMA.
-- 7 de septiembre de 2020
--
-- Apellidos, Nombre: Marcos Hidalgo Baños
-- -------------------------------------------------------------------

module StructuralSemantics2020 where

import           While2020

----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ABAJO
----------------------------------------------------------------------

-- representation of configurations for While

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s

isFinal :: Config -> Bool
isFinal (Final _) = True
isFinal _         = False

isInter :: Config -> Bool
isInter (Inter _ _) = True
isInter _           = False

update :: State -> Var -> Z -> State
update s x v y = if x == y then v else s y

-- representation of the transition relation <S, s> -> s'

sosStm :: Config -> Config

-- x := a

sosStm (Inter (Ass x a) s) = Final (update s x (aVal a s))
  where
    update s x v y = if x == y then v else s y

-- skip

sosStm (Inter Skip s) = Final s

-- s1; s2

sosStm (Inter (Comp ss1 ss2) s)
  | isFinal next = Inter ss2 s'
  where
    next = sosStm (Inter ss1 s)
    Final s' = next

sosStm (Inter (Comp ss1 ss2) s)
  | isInter next = Inter (Comp ss1' ss2) s'
  where
    next = sosStm (Inter ss1 s)
    Inter ss1' s' = next

-- if b then s1 else s2

sosStm (Inter (If b ss1 ss2) s)
  | bVal b s = Inter ss1 s

sosStm (Inter (If b ss1 ss2) s)
  | not (bVal b s) = Inter ss2 s

-- while b do s

sosStm (Inter (While b ss) s) =
  Inter (If b (Comp ss (While b ss)) Skip ) s

----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ARRIBA
----------------------------------------------------------------------

-- | Problema 2. swap
---------------------
{-
    [swap_sos]                        
                  < x := y ; y := z , s > => s'
                --------------------------------- where z = s x
                      < swap x y , s > => s'
-}

sosStm (Inter (Swap x y) s) = sosStm (Inter (Comp (Ass x (N (s y))) (Ass y (N z))) s)
  where
    z = s x

-- | Problema 3. for
--------------------
{- 
    [for_sos] 
                  
              -------------------------------------------------------------
                < for (S1;b;S2) S3, s > => < S1 ; while b do (S2;S3), s >                          
-}

nsStm (Inter (For ss1 b ss2 ss3) s) = sosStm (Inter (Comp ss1 (Comp ss2 (While b ss3))) s)

----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ABAJO
----------------------------------------------------------------------

-- | derivation sequences

type DerivSeq = [Config]

-- | Define a function 'derivSeq' that given a WHILE statement 'stm' and an
-- initial state 's' returns the corresponding derivation sequence:

derivSeq :: Stm -> State -> DerivSeq
derivSeq stm s = derivSeq' (Inter stm s)
    where
      derivSeq' config
                | isFinal config = [config]
                | otherwise = config : derivSeq' (sosStm config)


-- | The function 'showDerivSeq' returns a String representation  of
-- a derivation sequence 'dseq'. The 'vars' argument is a list of variables
-- that holds the variables to be shown in the state:

showDerivSeq :: [Var] -> DerivSeq -> String
showDerivSeq vars dseq = unlines (map showConfig dseq)
  where
    showConfig (Final s)    = "Final state:\n" ++ unlines (showVars s vars)
    showConfig (Inter ss s) = show ss ++ "\n" ++ unlines (showVars s vars)
    showVars s vs = map (showVal s) vs
    showVal s x = " s(" ++ x ++ ")= " ++ show (s x)
