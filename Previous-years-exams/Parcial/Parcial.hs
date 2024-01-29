module Parcial where

type Zero = Integer

data Z  = Zero
        |  Suc Z
        |  Pred Z
        |  Neg Z
        |  ZeroIf Z Z Z
        deriving (Show, Eq)

tres :: Z
tres = Suc (Suc (Suc Zero))

menosDos :: Z
menosDos = Pred (Pred Zero)

uno :: Z
uno = Suc (Pred (Suc (Suc (Pred Zero))))

dos :: Z
dos = Neg (Pred (Pred Zero))

menosUno :: Z
menosUno = ZeroIf (Pred (Pred Zero)) (Suc Zero) (Pred Zero)

-- a) foldZ
foldZ :: a -> (a -> a) -> (a -> a) -> (a -> a) -> (a -> a -> a -> a) -> Z -> a
foldZ fZero fSuc fPred fNeg fZeroIf exp = plegar exp
    where
        plegar Zero = fZero
        plegar (Suc z) = fSuc (plegar z)
        plegar (Pred z) = fPred (plegar z)
        plegar (Neg z) = fNeg (plegar z)
        plegar (ZeroIf z1 z2 z3) = 
            fZeroIf (plegar z1) (plegar z2) (plegar z3)

-- b)
eval :: Z -> Integer
eval exp = foldZ 0 (\ x -> x+1) (\ x -> x-1) (\ x -> -x) fIf exp
    where
        fIf z1 z2 z3 = if z1 == 0 then z2 else z3