-- -------------------------------------------------------------------
-- Structural Operational Semantics for WHILE 2021.
-- Examen de Lenguajes de Programación. UMA.
-- 1 de febrero de 2021
--
-- Apellidos, Nombre: Marcos Hidalgo Baños
-- -------------------------------------------------------------------

module StructuralSemantics where

-- En este fichero solo necesitas completar:
--
--   - 2.b la definición semántica de la sentencia case
--   - 2.b la implementación de la sentencia case
--
-- No modifiques el resto del código. Puedes probar
-- tu implementación con la función run, definida al final.

----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ABAJO
----------------------------------------------------------------------

-- import           Data.List.HT  (takeUntil)

import           While21
import           While21Parser

-- representation of configurations for While

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s
            | Stuck Stm State  -- <S, s>

isFinal :: Config -> Bool
isFinal (Final _) = True
isFinal _         = False

isInter :: Config -> Bool
isInter (Inter _ _) = True
isInter _           = False

isStuck :: Config -> Bool
isStuck (Stuck _ _) = True
isStuck _           = False

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
  | isStuck next = Stuck (Comp stm ss2) s'
  where
    next = sosStm (Inter ss1 s)
    Stuck stm s' = next

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

----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ARRIBA
----------------------------------------------------------------------

-- |----------------------------------------------------------------------
-- | Exercise 2.b
-- |----------------------------------------------------------------------

-- | Define the Structural Operational Semantics of the case statement.

{-
  ------------------------
  Regla 1 | LC ::= LL:S LC
  ------------------------
  |
  |  [Comentario: La expresion se encuentra en la fila actual y no es la ultima]
  |
  |  B[b]s == tt
  |  LL = n, LL
  |                                < S , s > => < S', s' > 
  |                -----------------------------------------------
  |              < if b then S else (case a of LL:S LC) , s > => < S', s' > 
  |              ----------------------------------------------------------- where b = aVal a s == n
  |                  < case a of (n,LL):S LC end, s > => < S', s' > 
  |  
  |
  |  [Comentario: La expresion se encuentra en la fila actual y es la ultima]
  |
  |  B[b]s == tt
  |  LL = n          
  |                                < S , s > => < S', s' > 
  |                ------------------------------------------------------
  |                < if b then S else (case a of LC) , s > => < S', s' > 
  |              --------------------------------------------------------- where b = aVal a s == n
  |                           < case a of n:S LC end, s > => s'
  |  
  | 
  |  [Comentario: La expresion NO se encuentra en la fila actual y hay mas en la misma fila]
  |
  |  B[b]s == ff
  |  LL = n, LL
  |                        < case a of LL:S LC, s > => < S', s' > 
  |              -----------------------------------------------------------
  |              < if b then S else (case a of LL:S LC) , s > => < S', s' > 
  |              ----------------------------------------------------------- where b = aVal a s == n
  |                   < case a of (n,LL):S LC end, s > => < S', s' > 
  |
  |
  |  [Comentario: La expresion NO se encuentra en la fila actual y hay NO mas en la misma fila]
  |  
  |  B[b]s == ff
  |  LL = n
  |                         < case a of LC , s > => < S', s' > 
  |                ------------------------------------------------------
  |                < if b then S else (case a of LC) , s > => < S', s' > 
  |               -------------------------------------------------------- where b = aVal a s == n
  |                         < case a of n:S LC end, s > => s'


  ---------------------
  Regla 2 | LC ::= LL:S
  --------------------- 
  |  
  |  [Comentario: La expresion se encuentra en la fila actual y no es la ultima]
  |
  |  B[b]s == tt
  |  LL = n, LL
  |                                < S , s > => < S', s' > 
  |                --------------------------------------------------------
  |                < if b then S else (case a of LL:S) , s > => < S', s' > 
  |              ----------------------------------------------------------- where b = aVal a s == n
  |                       < case a of (n,LL):S end, s > => < S', s' > 
  |  
  |
  |  [Comentario: La expresion se encuentra en la fila actual y es la ultima]
  |
  |  B[b]s == tt
  |  LL = n          
  |                             < S , s > => < S', s' > 
  |                    ----------------------------------------------
  |                     < if b then S else Abort , s > => < S', s' > 
  |                ---------------------------------------------------- where b = aVal a s == n
  |                       < case a of n:S end, s > => < S', s' > 
  |   
  |
  |  [Comentario: La expresion NO se encuentra en la fila actual y hay mas en la misma fila]
  |
  |  B[b]s == ff
  |  LL = n, LL
  |                        < case a of LL:S , s > => < S', s' > 
  |                ---------------------------------------------------------
  |                < if b then S else (case a of LL:S) , s > => < S', s' > 
  |              ----------------------------------------------------------- where b = aVal a s == n
  |                      < case a of (n,LL):S end, s > => < S', s' > 
  | 
  |
  |  [Comentario: La expresion NO se encuentra en la fila actual y hay NO mas en la misma fila]
  |  
  |  B[b]s == ff
  |  LL = n
  |                         < Abort s > => < S', s' > 
  |                ------------------------------------------------
  |                 < if b then S else Abort , s > => < S', s' > 
  |            ----------------------------------------------------- where b = aVal a s == n
  |                   < case a of n:S end, s > => < S', s' > 
  |

  ---------------------------
  Regla 3 | LC ::= default:S0
  --------------------------- 

    --------------------------------------------
     < case a of default:S0 , s > => < S0 , s >
   
-}

-- |----------------------------------------------------------------------
-- | Exercise 2.b
-- |----------------------------------------------------------------------

-- Abort
sosStm (Inter Abort s) = Stuck Abort s

-- | Implement in Haskell the Structural Semantics of the case statement.

sosStm (Inter (Case a (LabelledStm ll ss lc)) s)
 | elem (aVal a s) ll = Inter ss s
 | otherwise = sosStm (Inter (Case a lc) s)

sosStm (Inter (Case a (Default s0)) s) = Inter s0 s

sosStm (Inter (Case a EndLabelledStms) s) = Inter Abort s

----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ABAJO
----------------------------------------------------------------------

-- | Run the While program stored in filename and show final values of variables

run :: String -> IO()
run filename =
  do
     (_, vars, stm) <- parser filename
     let  dseq = derivSeq stm (const 0)
     putStr $ showDerivSeq vars dseq
     where
      derivSeq st ini = takeUntil (not . isInter) (iterate sosStm (Inter st ini))
      showDerivSeq vars dseq = unlines (map showConfig dseq)
         where
           showConfig (Final s) = "Final state:\n" ++ unlines (showVars s vars)
           showConfig (Stuck stm s) = "Stuck state:\n" ++ show stm ++ "\n" ++ unlines (showVars s vars)
           showConfig (Inter ss s) = show ss ++ "\n" ++ unlines (showVars s vars)
           showVars s vs = map (showVal s) vs
           showVal s x = " s(" ++ x ++ ")= " ++ show (s x)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs)
  | p x       = []
  | otherwise = x : takeUntil p xs
