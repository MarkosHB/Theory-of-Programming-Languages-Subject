----------------------------------------------------------------------
--
-- NaturalSemanticsWhile20.hs
-- Programming Languages
-- Final Exam. February, 2020
--
-- Natural Semantics for While20
--
-- Author: Marcos Hidalgo Baños
-- PC Number:
----------------------------------------------------------------------

module NaturalSemanticsWhile20 where

import           While20

----------------------------------------------------------------------
-- Memory Management (locations, store, updates,...)
----------------------------------------------------------------------

-- locations
type Loc = Integer

new :: Loc -> Loc
new l = l + 1

-- variable environment
type EnvVar = Var -> Loc

-- store
type Store = Loc -> Z

-- 'sto [next]' refers to the first available cell in the store 'sto'
next :: Loc
next = 0

-- update a variable environment with a new binding envV [x -> l]
updateVar :: EnvVar -> Var -> Loc -> EnvVar
updateVar envV x l = \ y -> if y == x then l else envV y

-- update a store with a new binding sto [l -> v]
updateStore :: Store -> Loc -> Z -> Store
updateStore sto l v = \ x -> if x == l then v else sto x

----------------------------------------------------------------------
-- Variable Declarations
----------------------------------------------------------------------

data ConfigD = InterD DecVar EnvVar Store  -- <Dv, envV, store>
             | FinalD EnvVar Store         -- <envV, store>

nsDecV :: ConfigD -> ConfigD

-- var x := a
nsDecV (InterD (Dec x a decs) envV store) =  nsDecV (InterD decs (updateVar envV x l) (updateStore (updateStore store l v) next (new l)))
  where
    l = store next
    v = aVal envV a store

-- epsilon
nsDecV (InterD EndDec envV store) = FinalD envV store

---------------------------------------------------------------------
-- Semantics of expressions
---------------------------------------------------------------------

type  Z      =  Integer
type  T      =  Bool

-- | Problema 1 - Semántica natural de Aexp y Bexp

{-
   Completa la definición de la semántica natural de Aexp.

   (Solo tienes que definir la semántica de numerales, variables, suma y let ... in)

      [NumNs] 
                EnvV |- < n, sto > --> Aexp N[n]

      [VarNs] 
                EnvV |- < x, sto > --> Aexp sto x
   
      [SumNs]  
               EnvV |- < a1, sto > --> Aexp z1 , EnvV |- <a2, sto > --> Aexp z2
               ----------------------------------------------------------------  where z3 = z1+z2
                        EnvV |- < a1 + a2, sto > --> Aexp z3

      [LetNs]  
               < Dv, EnvV[x-->l], sto[l-->v] > --> Aexp < EnvV', sto' > ; EnvV' |- < Let Dv in a, sto'> --> z
               ----------------------------------------------------------------------------------------------  where v = A[a, EnvV, sto] , l = sto next
                        EnvV |- < Let Dv in a, sto > --> Aexp z
 
-}

-- implementa aVal y bVal
aVal :: EnvVar -> Aexp -> Store -> Z
aVal envV exp sto = distinguir exp
   where
      distinguir (N n) = n
      distinguir (V x) = sto (envV x)
      distinguir (Add a1 a2) = (+) (distinguir a1) (distinguir a2)
      distinguir (Mult a1 a2) = (*) (distinguir a1) (distinguir a2)
      distinguir (Sub a1 a2) = (-) (distinguir a1) (distinguir a2)
      distinguir (Let (EndDec) a) = (distinguir a)
      distinguir (Let (Dec x a1 vrs) a2) = aVal envV' (Let vrs a2) sto'
         where 
            FinalD envV' sto' = nsDecV (InterD (Dec x a1 vrs) envV sto)

bVal :: EnvVar -> Bexp -> Store -> T
bVal envV exp sto = distinguir exp
   where
      distinguir (TRUE) = True
      distinguir (FALSE) = False
      distinguir (Eq a1 a2) = (==) (aVal envV a1 sto) (aVal envV a2 sto)
      distinguir (Le a1 a2) = (<=) (aVal envV a1 sto) (aVal envV a2 sto)
      distinguir (Neg b) = (not) (distinguir b)
      distinguir (And b1 b2) = (&&) (distinguir b1) (distinguir b2)    

---------------------------------------------------------------------
-- Execution of a program
---------------------------------------------------------------------

-- | Problema 2.a - execProgram

-- | 'execProgram' recibe un programa 'p', procesa sus variables globales
-- | para construir un entorno de variables y un store iniciales que almacenen
-- | las variables globales, y usa nsStm para obtener el store final. Devuelve
-- | una tupla con el entorno de variables global es y el store final.

execProgram :: Program -> (EnvVar, Store)
execProgram (Program pname dV ss) = undefined

-- | Problema 2.b - eliminación de variables locales

{-
    Explica en qué consiste la modificación que quieres hacer:

       ...

    Indica las reglas de la semántica que vas a modificar:

       ...

    Para cada regla modificada:

     1) anula mediante un comentario la implementación de la regla original para conservarla
     2) escribe en un comentario la definición formal de la nueva regla
     3) implementa la nueva regla justo debajo del comentario de su definición formal
-}

---------------------------------------------------------------------
-- Semantics of statements
---------------------------------------------------------------------

-- representation of configurations for While20

data Config = Inter Stm Store  -- <S, sto>
            | Final Store      -- sto

nsStm :: EnvVar -> Config -> Config

-- x := a
nsStm envV (Inter (Ass x a) sto) = Final sto'
   where
      sto' = updateStore sto (envV x) (aVal envV a sto)

-- skip
nsStm envV (Inter Skip sto) = Final sto

-- s1; s2
nsStm envV (Inter (Comp ss1 ss2) sto) = Final sto''
   where
      Final sto' = nsStm envV (Inter ss1 sto)
      Final sto'' = nsStm envV (Inter ss2 sto')

-- if b then s1 else s2
nsStm envV (Inter (If b ss1 ss2) sto) 
 | bVal envV b sto = nsStm envV (Inter ss1 sto)

nsStm envV (Inter (If b ss1 ss2) sto) 
 | not (bVal envV b sto) = nsStm envV (Inter ss2 sto)

-- while b do s
nsStm envV (Inter (While b s) sto) 
 | bVal envV b sto = Final sto''
   where 
      Final sto' = nsStm envV (Inter s sto)
      Final sto'' = nsStm envV (Inter (While b s) sto')

nsStm envV (Inter (While b s) sto) 
 | not (bVal envV b sto) = Final sto

-- | Problema 3 - loop s1 exit on b s2

{-
   Define  la semántica natural de loop:

   [loop_ns] 
   B[b]s == tt

         < S1 , s > --> s' , < S2 , s > --> s''
      -------------------------------------------
         < loop S1 exit when b S2 , s > --> s''

   [loop_ns] 
   B[b]s == ff

         < S1 , s > --> s' , < loop S1 exit when b S2, s' > --> s''
      --------------------------------------------------------------
               < loop S1 exit when b S2 , s > --> s''

-}

-- implementa la semántica de loop
-- B[b]s == tt
nsStm envV (Inter (Loop s1 b s2) sto)
 | bVal envV b sto = Final sto''
   where
      Final sto' = nsStm envV (Inter s1 sto)
      Final sto'' = nsStm envV (Inter s1 sto')

--B[b]s == ff
nsStm envV (Inter (Loop s1 b s2) sto)
 | not (bVal envV b sto) = nsStm envV (Inter (Loop s1 b s2) sto')
   where
      Final sto' = nsStm envV (Inter s1 sto)

-- block vars s
nsStm envV (Inter (Block dv ss) sto) = Final sto''
   where
      FinalD envV' sto' = nsDecV (InterD dv envV sto)
      Final sto'' = nsStm envV' (Inter ss sto')

-- | Problema 4 - selección no determinista

{-
   Define la semántica natural de la selección no determinista (no es
   necesario implementarla):

   [if_fi_ns]
   B[b]s == tt
                   < S or (if Gs fi), s > -> s' 
               -----------------------------------  
                 < (if b -> S; G​s fi), s > -> s'

   
   [if_fi_ns]
   B[b]s == ff
                     < (if Gs fi), s > -> s'
               ------------------------------------ 
                  < (if b -> S; G​s fi), s > -> s'
  
-}
