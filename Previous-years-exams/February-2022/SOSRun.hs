{-|

Programming Languages
Fall 2021

Author: Pablo López

-}

module SOSRun where

import           Aexp
import           Bexp
import           StructuralSemantics
import           While
import           WhileParser

type DerivSeq = [Config]

-- | Run the While program stored in 'filename' and show final values of output variables
run :: FilePath -> IO()
run filename =
  do
     (_, vars, stm) <- parser filename
     let  dseq = derivSeq stm (const 0)
     putStr $ showDerivSeq vars dseq

derivSeq :: Stm -> State -> DerivSeq
derivSeq st ini = derivSeq' (Inter st ini)
  where
    derivSeq' :: Config -> DerivSeq
    derivSeq' cf@(Final s)     = [cf]
    derivSeq' cf@(Stuck stm s) = [cf]
    derivSeq' cf@(Inter ss s)  = cf : derivSeq' (sosStm cf)

showDerivSeq :: [Var] -> DerivSeq -> String
showDerivSeq vars dseq = unlines (map showConfig dseq)
  where
    showConfig (Final s) = "Final state:\n" ++ unlines (showVars s vars)
    showConfig (Stuck stm s) = "Stuck state:\n" ++ show stm ++ "\n" ++ unlines (showVars s vars)
    showConfig (Inter ss s) = show ss ++ "\n" ++ unlines (showVars s vars)
    showVars s vs = map (showVal s) vs
    showVal s x = " s(" ++ x ++ ")= " ++ show (s x)
