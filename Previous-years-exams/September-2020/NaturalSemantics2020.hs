-- -------------------------------------------------------------------
-- Natural Semantics for WHILE 2020.
-- Examen de Lenguajes de Programación. UMA.
-- 7 de septiembre de 2020
--
-- Apellidos, Nombre: Marcos Hidalgo Baños
-- -------------------------------------------------------------------

module NaturalSemantics2020 where

import           While2020
import           Test.HUnit          hiding (State)

----------------------------------------------------------------------

-- | Problema 1. reduce
{- a) Definicion formal
    
 [N n] 
        reduce (N n) = N n

  [V x]
        reduce (V x) = V x

  [Add]                      { N (n1 + n2)                            if (isN a1) && (isN a2)
        reduce (Add a1 a2)   { reduce (Add (reduce a1) (reduce a2))   if isN (reduce a1) && isN (reduce a2)
                             { Add (reduce a1) (reduce a2)            otherwise

  [Mult]                     { N (n1 * n2)                            if (isN a1) && (isN a2)
        reduce (Mult a1 a2)  { reduce (Mult (reduce a1) (reduce a2))  if isN (reduce a1) && isN (reduce a2)
                             { Mult (reduce a1) (reduce a2)           otherwise

  [Sub]                      { N (n1 - n2)                            if (isN a1) && (isN a2)
        reduce (Sub a1 a2)   { reduce (Sub (reduce a1) (reduce a2))   if isN (reduce a1) && isN (reduce a2)
                             { Sub (reduce a1) (reduce a2)            otherwise
-}

isN :: Aexp -> Bool
isN (N _) = True
isN _ = False

-- b) Implementacion
reduce:: Aexp -> Aexp
reduce (N n) = N n
reduce (V x) = V x

--add
reduce (Add (N n1) (N n2)) = N (n1 + n2)
reduce (Add a1 a2)
 | isN exp1 && isN exp2 = reduce (Add exp1 exp2)
 | otherwise = Add exp1 exp2
  where
    exp1 = reduce a1 
    exp2 = reduce a2

--mult
reduce (Mult (N n1) (N n2)) = N (n1 * n2)
reduce (Mult a1 a2)
 | isN exp1 && isN exp2 = reduce (Mult exp1 exp2)
 | otherwise = Mult exp1 exp2
  where
    exp1 = reduce a1 
    exp2 = reduce a2

--sub
reduce (Sub (N n1) (N n2)) = N (n1 - n2)
reduce (Sub a1 a2)
 | isN exp1 && isN exp2 = reduce (Sub exp1 exp2)
 | otherwise = Sub exp1 exp2
  where
    exp1 = reduce a1 
    exp2 = reduce a2

testReduce :: Test
testReduce = test 
  [ Add (V "x") (N 15) ~=? reduce (Add (V "x") (Mult (N 5) (N 3))),
    Add (Mult (N 8) (V "y")) (N 11) ~=? reduce (Add (Mult (N 8) (V "y")) (Add (Mult (N 3) (N 2)) (N 5)))
  ]

{- c) Demostracion mantiene semantica
    
    Casos base 
    ----------
    N n                   V x
    | reduce (N n)        | reduce (V x)
    | {eq. 1}             | {eq. 2}
    | N n                 | V x
    
    Caso inductivo 
    --------------
    HI_Add --> paratodo a1, a2 perteneciente a Aexp,
               Add a1 a2 == reduce (Add a1 a2)

    Demostracion 1 (N n)              Demostracion 2 (Add a1 a2)                    Demostracion 3 (V x)
    --------------------              --------------------------                    --------------------
      Add a1 a2                         Add a1 a2                                     Add a1 a2
      | reduce (Add (N n1) (N n2))      | reduce (Add a1 a2)                          | reduce (Add exp1 exp2)
      | {eq. 3}                         | {eq. 4}                                     | {eq. 5}
      | N (n1 + n2)                     | reduce (Add (reduce a1) (reduce a2))        | Add (V x1) (V x2)
      | {HI}                            | {HI}                                        | {HI}                                
      | N n3 , where n3 = n1 + n2       | reduce (Add exp1 exp2)                      | Add a1 a2 , where a1=(V x1), a2=(V x2)
                                            , where exp1 = reduce a1
                                                    exp2 = reduce a2
    (de igual manera para Mult, Sub)

-}

----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ABAJO
----------------------------------------------------------------------

-- representation of configurations for While

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s

update :: State -> Var -> Z -> State
update s x v y = if x == y then v else s y

-- representation of the transition relation <S, s> -> s'

nsStm :: Config -> Config

-- x := a

nsStm (Inter (Ass x a) s) = Final (update s x (aVal a s))

-- skip

nsStm (Inter Skip s) = Final s

-- s1; s2

nsStm (Inter (Comp stm1 stm2) s) = Final s''
  where
    Final s'  = nsStm (Inter stm1 s)
    Final s'' = nsStm (Inter stm2 s')

-- if b then s1 else s2

nsStm (Inter (If b stm1 stm2) s)
  | bVal b s = Final s'
  where
    Final s' = nsStm (Inter stm1 s)

nsStm (Inter (If b stm1 stm2) s)
  | not(bVal b s) = Final s'
  where
    Final s' = nsStm (Inter stm2 s)

-- while b do s

nsStm (Inter (While b stm) s)
  | bVal b s = Final s''
  where
    Final s'  = nsStm (Inter stm s)
    Final s'' = nsStm (Inter (While b stm) s')

nsStm (Inter (While b stm) s)
   | not (bVal b s) = Final s

----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ARRIBA
----------------------------------------------------------------------

-- | Problema 2. swap
---------------------
{-
    [swap_ns]       
                  < x := y ; y := z , s > -> s'
                --------------------------------- where z = s x
                      < swap x y , s > -> s'
-}
nsStm (Inter (Swap x y) s) = nsStm (Inter (Comp (Ass x (N (s y))) (Ass y (N z))) s)
  where
    z = s x

-- | Problema 3. for
--------------------
{-
    [for_ns] 
                < S1 ; while b do (S2;S3), s > -> s'
              ---------------------------------------
                   < for (S1;b;S2) S3, s > -> s'                             
-}

nsStm (Inter (For ss1 b ss2 ss3) s) = nsStm (Inter (Comp ss1 (Comp ss2 (While b ss3))) s)

-- | Problema 4.  if b then (S1; S) else (S2; S) == if b then S1 else S2; S
---------------------------------------------------------------------------
{-

    Demostracion semantica natural  (Demostracion en  el otro sentido de igual manera)                               
    ------------------------------   
      Obtener Judgements a partir de:                                    
      if b then (S1; S) else (S2; S)
      |
      |  B[b]s = tt
      |                     < S1 , s > -> s'' , < S , s'' > -> s' 
      |                    ---------------------------------------
      |                             < S1; S , s > -> s' 
      |                ----------------------------------------------
      |                 < if b then (S1; S) else (S2; S) , s > -> s'
      |  
      |  B[b]s = ff
      |                     < S2 , s > -> s'' , < S , s'' > -> s' 
      |                    ---------------------------------------
      |                             < S2; S , s > -> s' 
      |                ----------------------------------------------
      |                 < if b then (S1; S) else (S2; S) , s > -> s'
      |
      Obtener la otra expresion:
      if b then S1 else S2; S
      |
      |  B[b]s = tt
      |                         < S1 , s > -> s''
      |                 -----------------------------------
      |                 < if b then S1 else S2 , s > -> s'' , < S , s'' > -> s' 
      |                ---------------------------------------------------------
      |                       < if b then S1 else S2; S , s > -> s'
      |  
      |  B[b]s = ff
      |                         < S2 , s > -> s''
      |                 -----------------------------------
      |                 < if b then S1 else S2 , s > -> s'' , < S , s'' > -> s' 
      |                ---------------------------------------------------------
      |                       < if b then S1 else S2; S , s > -> s'


    Demostracion semantica estructural  (Demostracion en  el otro sentido de igual manera)                               
    ----------------------------------                                    
      Si < if b then (S1; S) else (S2; S) , s > =>* s'
      entonces < if b then S1 else S2; S , s > =>* s'

      Asumo < if b then (S1; S) else (S2; S) , s > =>* s'
      |
      | Si B[b]s == tt
      |  |  
      |  | < S1; S , s > =>* s'
      |  | 
      |  | Aplicando Splitting
      |  | < S1 , s > =>* s''
      |  | < S , s''> =>* s'    
      |  | 
      |  | Aplicando [if_ns]
      |  | < if b then S1 else S2, s > =>* s''
      |  | 
      |  | Aplicando No Interferencia
      |  | < if b then S1 else S2 ; S, s > =>* s'         
      |
      | Si B[b]s == ff       
      |  |  
      |  | < S2; S , s > =>* s'
      |  | 
      |  | Aplicando Splitting
      |  | < S2 , s > =>* s''
      |  | < S , s''> =>* s'    
      |  | 
      |  | Aplicando [if_ns]
      |  | < if b then S1 else S2, s > =>* s''
      |  | 
      |  | Aplicando No Interferencia
      |  | < if b then S1 else S2 ; S, s > =>* s' 

-}

-- | Problema 5. iteración no determinista
------------------------------------------
{-

    [do_od_ns]
    B[b]s = tt
                    < S or (do Gs od) , s > -> s'
                  ---------------------------------
                    < do b -> S; Gs od , s > -> s'
    
    [do_od_ns]
    B[b]s = ff
                       < do Gs od , s > -> s'
                  ---------------------------------
                    < do b -> S; Gs od , s > -> s'
-}

----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ABAJO
----------------------------------------------------------------------

-- semantic function for natural semantics
sNs :: Stm -> State -> State
sNs ss s = s'
  where Final s' = nsStm (Inter ss s)

-- Example C.1
sFac = sNs factorial sInit
-- End Example C.1