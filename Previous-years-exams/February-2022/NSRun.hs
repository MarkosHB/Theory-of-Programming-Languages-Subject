{-|

Programming Languages
Fall 2021

Author: Pablo López

-}

module NSRun where

import           Aexp
import           NaturalSemantics
import           WhileParser

-- | Run the While program stored in 'filename' and show final values of output variables
run :: FilePath -> IO()
run filename =
  do
     (program, vars, stm) <- parser filename
     let s = sNs stm (const 0)
     putStrLn $ "Program " ++ program ++ " finalized."
     putStr "Final State: "
     print $ showState s vars

showState :: State -> [Var] -> [String]
showState s = map (\ x -> x ++ "->" ++ show (s x))
