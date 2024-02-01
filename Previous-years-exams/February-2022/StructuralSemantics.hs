{-|

Teoría de los Lenguajes de Programación
Enero 2022

Apellidos, Nombre: Marcos Hidalgo Baños
Código PC:

-}

module StructuralSemantics where

import           Aexp
import           Bexp
import           While

update :: State -> Var -> Z -> State
update s x v y = if x == y then v else s y

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

-----------------------------------------------------------
-- | Ejercicio 1.b - sentencia bloque sin variables locales
-----------------------------------------------------------

{- Define aquí la semántica estructural de block sin variables:


-}

-- Implementa aquí la semántica estructural de block.

sosStm (Inter (Block dV ss) s) = Inter ss s

-------------------------------------
-- | Ejercicio 2.b - sentencia select
-------------------------------------

{- Define aquí la semántica estructural de select:

  [select_sos]
  B[b]s == tt
  SC ::= b S sc
       | b S

                            < S , s > => < S' , s' >
                ---------------------------------------------------
                    < select when b do S sc , s > => < S' , s' >


  [select_sos]
  B[b]s == ff
  SC ::= b S sc

                        < select sc , s > => < S', s' >
                -----------------------------------------------
                  < select when b do S sc , s > => < S', s' >

  [select_sos]
  B[b]s == ff
  SC ::= b S
                    
                -------------------------------------------------------------------
                  < select when b do S , s > -> < Stuck (select when b do S), s >

-}

-- Implementa aquí la semántica estructural de select.

sosStm (Inter (Select (Case b ss sc)) s) 
 | bVal b s = Inter (ss) s

sosStm (Inter (Select (Case b ss sc)) s)
 | not (bVal b s) = Inter (Select sc) s 

sosStm (Inter (Select (EndCase b ss)) s)
 | bVal b s = Inter (ss) s

sosStm (Inter (Select (EndCase b ss)) s)
 | not (bVal b s) = Stuck (Select (EndCase b ss)) s

----------------------------------
-- | Ejercicio 3.b - sentencia for
----------------------------------

{- Define aquí la semántica estructural de for:

-}

-- Implementa aquí la semántica estructural de for.

sosStm (Inter (For ss1 b ss2 ss) s) = undefined


-----------------------------------------------------------------------------
-- | Ejercicio 5.  if b then (S1;S) else (S2;S)  ==  if b then S1 else S2 ; S
-----------------------------------------------------------------------------
{- 
   Demuestra que son semanticamente equivalentes en la semantica estructural
   operacional. Solo demostrar el caso de la guarda cierta.

   Demostracion 1
   --------------
      Si < if b then (S1;S) else (S2;S) , s > =>* s'
      entonces < if b then S1 else S2 ; S , s > =>* s'
      donde B[b]s == tt

      Suponemos < if b then (S1;S) else (S2;S) , s > =>* s'
      Aplicando [if_sos_tt]
      < S1 ; S , s > =>* s'

      Aplicando Splitting:
      < S1 , s > =>* s''
      < S , s''> => s'

      Aplicando [if_sos_tt] construimos:
      < if b then S1 else S2 , s > =>* s''
      < S , s''> => s'

      Aplicando No Interferencia:
      < if b then S1 else S2 ; S , s > =>* s'


   Demostracion 2
   --------------
      Si < if b then S1 else S2 ; S , s > =>* s'
      entonces < if b then (S1;S) else (S2;S) , s > =>* s' 
      donde B[b]s == tt

      Suponemos < if b then S1 else S2 ; S , s > =>* s'
      Aplicando Splitting:
      < if b then S1 else S2 , s > =>* s''
      < S , s'' > =>* s'

      Aplicando [if_sos_tt]
      < S1, s > => s''
      < S , s'' > =>* s'

      Aplicando No Interferencia (en ambas ramas)
      < S1 ; S , s > =>* s'

      Aplicando [if_sos_tt] construimos (en ambas ramas)
      < if b then (S1;S) else (S2;S) , s > =>* s' 

-}