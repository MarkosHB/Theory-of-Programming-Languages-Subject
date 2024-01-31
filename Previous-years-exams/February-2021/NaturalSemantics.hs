-- -------------------------------------------------------------------
-- Natural Semantics for WHILE 2021.
-- Examen de Lenguajes de Programación. UMA.
-- 1 de febrero de 2021
--
-- Apellidos, Nombre: Marcos Hidalgo Baños
-- -------------------------------------------------------------------

module NaturalSemantics where

-- En este fichero solo necesitas completar:
--
--   - 2.a la definición semántica de la sentencia case
--   - 2.a la implementación de la sentencia case
--
-- No modifiques el resto del código. Puedes probar
-- tu implementación con la función run, definida al final.

----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ABAJO
----------------------------------------------------------------------

import           While21
import           While21Parser

updateState :: State -> Var -> Z -> State
updateState s x v y = if x == y then v else s y

-- representation of configurations for While

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s

-- representation of the transition relation <S, s> -> s'

nsStm :: Config -> Config

-- x := a

nsStm (Inter (Ass x a) s) = Final (updateState s x (aVal a s))

-- skip

nsStm (Inter Skip s) = Final s

-- s1; s2

nsStm (Inter (Comp ss1 ss2) s) = Final s''
  where
    Final s'  = nsStm (Inter ss1 s)
    Final s'' = nsStm (Inter ss2 s')

-- if b then s1 else s2

nsStm (Inter (If b ss1 ss2) s)
  | bVal b s = Final s'
  where
    Final s' = nsStm (Inter ss1 s)

nsStm (Inter (If b ss1 ss2) s)
  | not(bVal b s) = Final s'
  where
    Final s' = nsStm (Inter ss2 s)

----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ARRIBA
----------------------------------------------------------------------

-- case a of

-- |----------------------------------------------------------------------
-- | Exercise 2.a
-- |----------------------------------------------------------------------

-- | Define the Natural Semantics of the case statement.

{-

  Completa la definición semántica de la sentencia case.

  ------------------------
  Regla 1 | LC ::= LL:S LC
  ------------------------
  |
  |  [Comentario: La expresion se encuentra en la fila actual y no es la ultima]
  |
  |  B[b]s == tt
  |  LL = n, LL
  |                                < S , s > -> s'
  |                -----------------------------------------------
  |              < if b then S else (case a of LL:S LC) , s > -> s'
  |              -------------------------------------------------- where b = aVal a s == n
  |                  < case a of (n,LL):S LC end, s > -> s'
  |  
  |
  |  [Comentario: La expresion se encuentra en la fila actual y es la ultima]
  |
  |  B[b]s == tt
  |  LL = n          
  |                                < S , s > -> s'
  |                -----------------------------------------------
  |                < if b then S else (case a of LC) , s > -> s'
  |              -------------------------------------------------- where b = aVal a s == n
  |                  < case a of n:S LC end, s > -> s'
  |  
  | 
  |  [Comentario: La expresion NO se encuentra en la fila actual y hay mas en la misma fila]
  |
  |  B[b]s == ff
  |  LL = n, LL
  |                        < case a of LL:S LC, s > -> s'
  |              --------------------------------------------------
  |              < if b then S else (case a of LL:S LC) , s > -> s'
  |              -------------------------------------------------- where b = aVal a s == n
  |                   < case a of (n,LL):S LC end, s > -> s'
  |
  |
  |  [Comentario: La expresion NO se encuentra en la fila actual y hay NO mas en la misma fila]
  |  
  |  B[b]s == ff
  |  LL = n
  |                        < case a of LC , s > -> s'
  |                ------------------------------------------------
  |                < if b then S else (case a of LC) , s > -> s'
  |              -------------------------------------------------- where b = aVal a s == n
  |                   < case a of n:S LC end, s > -> s'


  ---------------------
  Regla 2 | LC ::= LL:S
  --------------------- 
  |  
  |  [Comentario: La expresion se encuentra en la fila actual y no es la ultima]
  |
  |  B[b]s == tt
  |  LL = n, LL
  |                                < S , s > -> s'
  |                -----------------------------------------------
  |                < if b then S else (case a of LL:S) , s > -> s'
  |              -------------------------------------------------- where b = aVal a s == n
  |                  < case a of (n,LL):S end, s > -> s'
  |  
  |
  |  [Comentario: La expresion se encuentra en la fila actual y es la ultima]
  |
  |  B[b]s == tt
  |  LL = n          
  |                                < S , s > -> s'
  |                -----------------------------------------------
  |             < if b then S else error "No registrado" , s > -> s'
  |             ---------------------------------------------------- where b = aVal a s == n
  |                  < case a of n:S end, s > -> s'
  |   
  |
  |  [Comentario: La expresion NO se encuentra en la fila actual y hay mas en la misma fila]
  |
  |  B[b]s == ff
  |  LL = n, LL
  |                        < case a of LL:S , s > -> s'
  |                ------------------------------------------------
  |                < if b then S else (case a of LL:S) , s > -> s'
  |              -------------------------------------------------- where b = aVal a s == n
  |                   < case a of (n,LL):S end, s > -> s'
  | 
  |
  |  [Comentario: La expresion NO se encuentra en la fila actual y hay NO mas en la misma fila]
  |  
  |  B[b]s == ff
  |  LL = n
  |                         error "No registrado" -> s'
  |                ------------------------------------------------
  |            < if b then S else error "No registrado" , s > -> s'
  |            ----------------------------------------------------- where b = aVal a s == n
  |                   < case a of n:S end, s > -> s'
  |

  ---------------------------
  Regla 3 | LC ::= default:S0
  --------------------------- 

            < S0 , s > -> s'
    ------------------------------------
     < case a of default:S0 , s > -> s' 
   
-}

-- |----------------------------------------------------------------------
-- | Exercise 2.a
-- |----------------------------------------------------------------------

-- | Implement the Natural Semantics of the case statement.

nsStm (Inter (Case a (LabelledStm ll ss lc)) s)
 | elem (aVal a s) ll = nsStm (Inter ss s)
 | otherwise = nsStm (Inter (Case a lc) s) 

nsStm (Inter (Case a (Default s0)) s) = nsStm (Inter s0 s)

nsStm (Inter (Case a (EndLabelledStms)) s) = error "No registrado"

----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ABAJO
----------------------------------------------------------------------

-- | Run the While program stored in filename and show final values of variables.
--   For example: run "TestCase.w"

run :: String -> IO()
run filename =
  do
     (_, vars, stm) <- parser filename
     let Final s = nsStm (Inter stm (const 0))
     print $ showState s vars
     where
       showState s = map (\ v -> v ++ "->" ++ show (s v))

-- |--------------------------------------------
-- | Exercise 3 |      |FV(a{x|->y})| <= |FV(a)|
-- |--------------------------------------------
{-
    Recordemos que:

    FV (N n)        = Empty               
    FV (V x)        = {x}                              
    FV (Add a1 a2)  = (FV a1) U (FV a2)   
    FV (Mult a1 a2) = (FV a1) U (FV a2)   
    FV (Sub a1 a2)  = (FV a1) U (FV a2)   
     
    Casos base
    ----------
    a = N n                  a = V x                a = V z
     |                        |                      |
     |  |FV((N n){x|->y})|    | |FV((V x){x|->y})|   | |FV((V z){x|->y})| 
     |  {def. sustitucion}    | {def. sustitucion}   | {def. sustitucion}
     |  |FV(N n)              | |FV(V y)|            | |FV(V z)|            
     |  {eq. 1}               | {eq. 2}              | {eq. 2}
     |  |Empty|               | |{y}|                | |{z}|
     |  {def. cardinal}       | {def. cardinal}      | {def. cardinal}
     |  0 = |FV(N n)|         | 1 = |FV(V x)|        | 1 = |FV(V z)|
   
   Casos Inductivos (De igual manera para todos)
   ----------------
    a = Add a1 a2
     | 
     | |FV((Add a1 a2){x|->y})|
     | {def. sustitucion}
     | |FV (a1{x|->y} + a2{x|->y})|  
     | {eq. 3}
     | |FV (a1{x|->y}| U |FV (a2{x|->y}|  
     | {def. cardinal}
     | |FV (a1{x|->y}| + |FV (a2{x|->y}| 
     | {HI}
     | z1 + z2 <= z3 
          , where z3 = |FV(a)|
                  z1 = |FV (a1{x|->y}|
                  z2 = |FV (a2{x|->y}|

     HI -> paratodo a perteneciente a Aexp,
           |FV(a{x|->y})| <= |FV(a)|

-}

-- |-----------------------------------------------------------------------
-- | Exercise 4 | if b then (S1; S) else (S2; S) == S; if b then S1 else S2
-- |-----------------------------------------------------------------------
{-

    Demostracion semantica natural  (Demostracion en  el otro sentido de igual manera)                               
    ------------------------------   
      Obtener Judgements a partir de:                                    
      if b then (S; S1) else (S; S2)
      |
      |  B[b]s = tt
      |                     < S , s > -> s'' , < S1 , s'' > -> s' 
      |                    ---------------------------------------
      |                             < S; S1 , s > -> s' 
      |                ----------------------------------------------
      |                 < if b then (S; S1) else (S; S2) , s > -> s'
      |  
      |  B[b]s = ff
      |                     < S , s > -> s'' , < S2 , s'' > -> s'
      |                    ---------------------------------------
      |                             < S; S2 , s > -> s' 
      |                ----------------------------------------------
      |                 < if b then (S; S1) else (S; S2) , s > -> s'
      |
      Obtener la otra expresion:
      S; if b then S1 else S2
      |
      |  B[b]s = tt
      |                                               < S1 , s'' > -> s'
      |                                      -----------------------------------
      |                  < S , s > -> s'' , < if b then S1 else S2 , s'' > -> s'
      |                ---------------------------------------------------------
      |                       < S; if b then S1 else S2 , s > -> s'
      |  
      |  B[b]s = ff
      |                                               < S2 , s'' > -> s'
      |                                      -----------------------------------
      |                  < S , s > -> s'' , < if b then S1 else S2 , s'' > -> s'
      |                ---------------------------------------------------------
      |                       < S; if b then S1 else S2 , s > -> s'
-}

