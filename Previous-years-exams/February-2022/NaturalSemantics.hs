{-|

Teoría de los Lenguajes de Programación
Enero 2022

Apellidos, Nombre: Marcos Hidalgo Baños
Código PC:

-}

module NaturalSemantics where

import           Aexp
import           Bexp
import           While

update :: State -> Var -> Z -> State
update s x v y = if x == y then v else s y

----------------------------------------------------------------------
-- Natural Semantics for Variable Declarations
----------------------------------------------------------------------

{-

WHILE 22 es un lenguaje imperativo, estructurado en bloques y con ambito dinamico
cuya sintaxis abstracta es:

S ::= x:=a | skip | S1; S2
| begin Dv S end
| select Cs end
| for(S1;b;S2) do S3

Dv ::= var x:= a; Dv | Epsilon

Cs ::= when b do S Cs | when b do S

La sentencia bloque begin Dv S end introduce las variables locales declaradas en Dv, 
cuyo ambito queda restringido al cuerpo del bloque, S. 
Tras ejecutar el siguiente programa WHILE 22:

x:=1;
y:=2; 
begin 
   var z:=x+y;
   var x:= 50;

   y:=x+z;
   x:=2*x;
   w:=x+y+z
end

el estado final es [x->1,y->53,z->0,w->156]

a.- Implementa la semantica natural de la sentencia bloque (Pag 42 tanspraencias viene la definicion)

Para implementar esta regla necesitarás implementar la semantica natural 
de la declaración de variables y la generalización del update (tranparencia 40 y 41)

Puedes probar tu implementación con testblock.w

b.- Para el caso de la semántica estructural operacional, 
consideraremos que la sentencia bloque nunca introduce variables (Dv = Epsilon).
Define e implementa la semantica estructural operacional. 
Para probar la implementación con TestBloqueWithoutVars.w

-} 

---------------------------------------------------------------------
-- | Ejercicio 1.a - Semántica natural de la declaración de variables
---------------------------------------------------------------------

-- Define aquí  los tipos y  funciones necesarios para  implementar la
-- semántica   natural   de  la   declaración   de   variables  y   la
-- generalización del update.

norepe :: [Var] -> [Var]
norepe [] = []
norepe (x:xs) 
 | elem x xs = norepe xs
 | otherwise = x:(norepe xs)

dv :: DecVar -> [Var]
dv (EndDec) = []
dv (Dec v s ls) = norepe ([v] ++ (dv ls))

updateGeneral :: State -> State -> [Var] -> Var -> Z
updateGeneral s s' ls x  
 | elem x ls = s x
 | otherwise = s' x

----------------------------------------------------------------------
-- Natural Semantics for While
----------------------------------------------------------------------

-- representation of configurations for While

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s

-- representation of the transition relation <S, s> -> s'

nsStm :: Config -> Config

-- x := a

nsStm (Inter (Ass x a) s) = Final (update s x (aVal a s))

-- skip

nsStm (Inter Skip s) = Final s

-- s1; s2

nsStm (Inter (Comp ss1 ss2) s) = Final s''
  where
    Final s'  = nsStm (Inter ss1 s)
    Final s'' = nsStm (Inter ss2 s')

-------------------------------------
-- | Ejercicio 1.a - sentencia bloque
-------------------------------------

-- Implementa  aquí  la  semántica  natural de  la  sentencia  bloque.
-- Necesitarás  implementar  más arriba  la  semántica  natural de  la
-- declaración de variables y la generalización del update.

nsStm (Inter (Block dV ss) s) = undefined
-------------------------------------
-- | Ejercicio 2.a - sentencia select
-------------------------------------
{-

La sentencia condicional select selecciona y ejecuta la primera sentencia
condicional cuya guarda sea cierta. Tras el programa WHILE22:

x:=2
select 
   when x = 1 do x:= 50
   when x = 2 do x:= 60
   when x = 3 do x:= 70
end

el estado final es [x->60]. Para ejecutar select se comprueban las guardas
de las sentencias condicionales en orden de aparición y se ejecuta la primera
sentencia condicional cuya guarda sea cierta, el resto de sentencia condicionales
se descartan. Si ninguna es cierta el programa aborta.
Define e implementa nsStm y sosStm de select

-}

{- Define aquí la semántica natural de select:

  [select_ns]
  B[b]s == tt
  SC ::= b S sc
       | b S
                            < S , s > -> s'
                ----------------------------------------
                    < select when b do S sc , s > -> s'


  [select_ns]
  B[b]s == ff
  SC ::= b S sc

                        < select sc , s > -> s'
                ----------------------------------------
                  < select when b do S sc , s > -> s'

  [select_ns]
  B[b]s == ff
  SC ::= b S
                ------------------------------------------------------------
                  < select when b do S , s > -> error "No coincide ninguna"

-}

-- Implementa aquí la semántica natural de select.

nsStm (Inter (Select (Case b ss sc)) s) 
 | bVal b s = nsStm (Inter (ss) s) 

nsStm (Inter (Select (Case b ss sc)) s)
 | not (bVal b s) = nsStm (Inter (Select sc) s)  

nsStm (Inter (Select (EndCase b ss)) s)
 | bVal b s = nsStm (Inter (ss) s) 

nsStm (Inter (Select (EndCase b ss)) s)
 | not (bVal b s) = error "No coincide ninguna"

----------------------------------
-- | Ejercicio 3.a - sentencia for
----------------------------------

{- La sentencia iterativa for es similar al bucle for de los lenguajes java/c/c++.

Recuerda que su sintaxis es: 
  S ::= for(S1;b;S2);S3

donde S1 es una sentencia que se evalúa siempre una sola vez antes de la
ejecución del bucle; b es la guarda precomprobada del bucle, S3 es el cuerpo
del bucle y S2 es una sentencia que se ejecuta siempre despues de cada iteración
del bucle. Tras ejecutar el programa WHILE22

s:= 0;
for(x:=0; x<=3 ; x:=x+1) do 
  s:=s+x

el estado final es [x->4,s->6]

Define nsStm y sosStm para for

Puedes comprobar las implementaciones con TestFor.w, Factorial.s y Divide.w

-}

{- Define aquí la semántica natural de for:

  [for_ns]
  B[b]s' == tt                                   
                    < S1 , s > -> s' , < S3 , s' > -> s'' , < S2 , s'' > -> s''' , < for (S4;b;S2) do S3 , s''' > -> s''''
                  ---------------------------------------------------------------------------------------------------------
                                              < for (S1;b;S2) do S3 , s > -> s''''

        where S1 = Ass v a
              S4 = Ass v (s''' v)
              

  [for_ns]
  B[b]s' == ff             < S1 , s > -> s'
                  ---------------------------------------
                      < for (S1;b;S2) do S3 , s > -> s'

-}

-- Implementa aquí la semántica natural de for.

nsStm (Inter (For ss1@(Ass v a) b ss2 ss3) s) 
 | (bVal b s') = Final s''''
  where 
    Final s' = nsStm (Inter ss1 s) 
    Final s'' = nsStm (Inter ss3 s')
    Final s''' = nsStm (Inter ss2 s'')
    ss4 = Ass v (N (s''' v))
    Final s'''' = nsStm (Inter (For ss4 b ss2 ss3) s)

nsStm (Inter (For ss1 b ss2 ss3) s) 
 | not (bVal b s') = Final s'
  where
    Final s' = nsStm (Inter ss1 s) 

-- semantic function for natural semantics
sNs :: Stm -> State -> State
sNs ss s = s'
  where Final s' = nsStm (Inter ss s)


-------------------------------------------------------------------------------
-- | Ejercicio 4. repeat S until b  ==  S; if b then skip else repeat S until b
-------------------------------------------------------------------------------
{- La semantica natural de repeat se define como:

                          < S, s > -> s'  
  [repeat_tt]    --------------------------------- if B[b]s' == tt
                    < repeat S until b, s > -> s' 
                
                  < S, s > -> s' < repeat S until b, s'> -> s''
  [repeat_ff]    ---------------------------------------------- if B[b]s' == ff
                  < repeat S until b, s > -> s''

  Demuestra que son semanticamente equivalentes. 
  En la demostración puedes abreviar repeat S until b como r S u b.


  Demostracion  (de manera similar en el otro sentido)
  ------------
                          < S, s > -> s'
    B[b]s' == tt      ----------------------
                       < r S u b , s > -> s'

       
                       < S, s > -> s' , < r S u b , s' > -> s''
    B[b]s' == ff      -----------------------------------------
                             < r S u b , s > -> s''
  

  Empleamos los judgments obtenidos para construir la otra expresion:

                                                    < Skip , s' > -> s'
                                         ------------------------------------------
                        < S, s > -> s' , < if b then skip else r S u b , s' > -> s'
    B[b]s' == tt      ----------------------------------------------------------------
                            < S; if b then skip else r S u b , s > -> s'


                                                  < r S u b , s' > -> s''
                                          ------------------------------------------
                        < S, s > -> s' , < if b then skip else r S u b , s' > -> s''
    B[b]s' == ff      ---------------------------------------------------------------
                           < S; if b then skip else r S u b , s > -> s''

-}
