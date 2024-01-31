-- -------------------------------------------------------------------
-- Structural Operational Semantics for Aexp.
-- Examen de Lenguajes de Programación. UMA.
-- 1 de febrero de 2021
--
-- Apellidos, Nombre: Marcos Hidalgo Baños
-- -------------------------------------------------------------------

module AexpSOS where

-- En este fichero solo necesitas completar:
--
--   - 1.a la definición semántica de Aexp
--   - 1.b la implementación de la semántica de Aexp
--   - 1.c la implementación de la secuencia de derivación
--
-- No modifiques el resto del código. Puedes probar
-- tu implementación con la función eval, definida al final.

import           While21

-- |----------------------------------------------------------------------
-- | Exercise 1.a
-- |----------------------------------------------------------------------

-- | Define the Structural Operational Semantics of Aexp extended with
--   integer division.

{-

    Completa la definición semántica de Aexp detallando reglas y axiomas
    con judgements de la forma <a,s> => <a',s>  y  <a,s> => n.

    [N n]
            < N n , s > => n


    [V x]
            < V x , s > => < s x, s >


    [Add a1 a2]
                    
          --------------------------------------- where n3 = n1 + n2
           < Add (N n1) (N n2), s > => < n3, s >

                    < a2 , s > => < a2' , s >
          -------------------------------------------------
           < Add (N n1) a2 , s > => < Add (N n1) a2' , s >  

                < a1 , s > => < a1' , s >
          -----------------------------------------
           < Add a1 a2 , s > => < Add a1' a2 , s > 


    [Div a1 a2]
                    
          --------------------------------------- where n3 = n1 / n2
           < Div (N n1) (N n2), s > => < n3, s >

                    < a2 , s > => < a2' , s >
          -------------------------------------------------
           < Div (N n1) a2 , s > => < Div (N n1) a2' , s >  

                < a1 , s > => < a1' , s >
          -----------------------------------------
           < Div a1 a2 , s > => < Div a1' a2 , s > 
-}

-- |----------------------------------------------------------------------
-- | Exercise 1.b
-- |----------------------------------------------------------------------

-- | Implement the Structural Operational Semantics for the
-- | evaluation of arithmetic expressions Aexp.

-- | Use the algebraic data type 'AexpConfig' to represent the
-- | configuration of the transition system

data AexpConfig = Redex Aexp State  -- a redex is a reducible expression
                | Stuck Aexp State  -- a stuck is neither reducible nor a value
                | Value Z           -- a value is not reducible; it is in normal form

isValue :: AexpConfig -> Bool
isValue (Value _) = True
isValue _         = False

isRedex :: AexpConfig -> Bool
isRedex (Redex _ _) = True
isRedex _           = False

isStuck :: AexpConfig -> Bool
isStuck (Stuck _ _) = True
isStuck _           = False

-- | Define a function 'sosAexp' that given a configuration 'AexpConfig'
-- | evaluates the expression in 'AexpConfig' one step and returns the
-- | next configuration.

sosAexp :: AexpConfig -> AexpConfig
sosAexp (Redex (N n) s) = Value n
sosAexp (Redex (V x) s) = Redex (N (s x)) s

--add
sosAexp (Redex (Add (N n1) (N n2)) s) = Redex (N n3) s
  where
    n3 = n1 + n2
  
sosAexp (Redex (Add (N n1) a2) s) = Redex (Add (N n1) a2') s
  where
    Redex a2' s = sosAexp (Redex a2 s)

sosAexp (Redex (Add a1 a2) s) = Redex (Add a1' a2) s
  where
    Redex a1' s = sosAexp (Redex a1 s)

--mult
sosAexp (Redex (Mult (N n1) (N n2)) s) = Redex (N n3) s
  where
    n3 = n1 * n2
  
sosAexp (Redex (Mult (N n1) a2) s) = Redex (Mult (N n1) a2') s
  where
    Redex a2' s = sosAexp (Redex a2 s)

sosAexp (Redex (Mult a1 a2) s) = Redex (Mult a1' a2) s
  where
    Redex a1' s = sosAexp (Redex a1 s)

--sub
sosAexp (Redex (Sub (N n1) (N n2)) s) = Redex (N n3) s
  where
    n3 = n1 - n2
  
sosAexp (Redex (Sub (N n1) a2) s) = Redex (Sub (N n1) a2') s
  where
    Redex a2' s = sosAexp (Redex a2 s)

sosAexp (Redex (Sub a1 a2) s) = Redex (Sub a1' a2) s
  where
    Redex a1' s = sosAexp (Redex a1 s)

--div
sosAexp (Redex (Div (N n1) (N n2)) s)
 | n2 == 0 = Stuck (Div (N n1) (N n2)) s
 | otherwise = Redex (N n3) s
  where
    n3 = div n1 n2
  
sosAexp (Redex (Div (N n1) a2) s) = Redex (Div (N n1) a2') s
  where
    Redex a2' s = sosAexp (Redex a2 s)

sosAexp (Redex (Div a1 a2) s) = Redex (Div a1' a2) s
  where
    Redex a1' s = sosAexp (Redex a1 s)

-- |----------------------------------------------------------------------
-- | Exercise 1.c
-- |----------------------------------------------------------------------

-- | Given the type synonym 'AexpDerivSeq' to represent derivation sequences
-- | of the structural operational semantics for arithmetic expressions 'Aexp':

type AexpDerivSeq = [AexpConfig]

-- | Define a recursive function 'aExpDerivSeq' that given a 'Aexp'
--   expression 'a' and an initial state 's' returns the corresponding
--   derivation sequence:

aExpDerivSeq :: Aexp -> State -> AexpDerivSeq
aExpDerivSeq a s 
 | isValue next = [Redex a s] ++ [Value n]
  where
    next = sosAexp (Redex a s)
    Value n = next

aExpDerivSeq a s 
 | isStuck next = [Redex a s] ++ [Stuck a' s']
  where
    next = sosAexp (Redex a s)
    Stuck a' s' = next

aExpDerivSeq a s 
 | isRedex next = [Redex a s] ++ (aExpDerivSeq a' s')
  where
    next = sosAexp (Redex a s)
    Redex a' s' = next
----------------------------------------------------------------------
-- NO MODIFICAR EL CODIGO DE ABAJO
----------------------------------------------------------------------

eval :: Aexp -> State -> IO()
eval a s = putStrLn $ showAexpDerivSeq ["x", "y", "z"] (aExpDerivSeq a s)
  where
    showAexpDerivSeq vars dseq = unlines (map showConfig dseq)
      where
        showConfig (Value n) = "Final value:\n" ++ show n
        showConfig (Stuck e st) = "Stuck expression:\n" ++ show e ++ "\n" ++ unlines (map (showVal st) vars)
        showConfig (Redex e st) = show e ++ "\n" ++ unlines (map (showVal st) vars)
        showVal st x = " s(" ++ x ++ ")= " ++ show (st x)

-- Tests
 
-- test your implementation with the examples below, for example:
-- eval exp1 sInit

sInit :: State
sInit "x" = 1
sInit "y" = 2
sInit "z" = 4
sInit  _  = 0

exp1 :: Aexp
exp1 = ( (V "x") `Add` (V "y") ) `Add` (V "z") -- (x + y) + z

exp2 :: Aexp
exp2 =  (V "x") `Add` ( (V "y") `Add` (V "z") ) -- x + (y + z)

exp3 :: Aexp
exp3 = Mult (V "x") (Add (V "y") (Sub (V "z") (N 1))) -- x * (y + (z - 1))

exp4 :: Aexp
exp4 = Mult (Add (V "x") (V "y")) (Sub (N 9) (V "z")) -- (x + y) * (9 - z)

exp5 :: Aexp
exp5 = Div (Mult (V "y") (V "z")) (Add (V "x") (N 1)) -- (y * z) / (x + 1)

exp6 :: Aexp
exp6 = Div (Mult (V "y") (V "z")) (Sub (V "x") (N 1)) -- (y * z) / (x - 1)
