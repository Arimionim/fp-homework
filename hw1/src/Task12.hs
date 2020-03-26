module Task12 where

data Nat = Z | S Nat deriving Show

sumNat :: Nat -> Nat -> Nat
sumNat a Z = a
sumNat a (S b) = S $ sumNat a b

mulNat :: Nat -> Nat -> Nat
mulNat _ Z = Z
mulNat a (S b) = sumNat a (mulNat a b)

subNat :: Nat -> Nat -> Nat
subNat a Z = a
subNat Z _ = Z
subNat (S a) (S b) = subNat a b

natToInt :: Nat -> Integer
natToInt Z = 0
natToInt (S a) = 1 + (natToInt a)

intToNat :: Integer -> Nat
intToNat 0 = Z
intToNat a = S (intToNat (a - 1))

equals :: Nat -> Nat -> Bool
equals a b = (compareNat a b) == 0

compareNat :: Nat -> Nat -> Integer
compareNat Z Z = 0
compareNat Z _ = -1
compareNat _ Z = 1
compareNat (S a) (S b) = compareNat a b

checkEven :: Nat -> Bool
checkEven Z = True
checkEven (S Z) = False
checkEven (S (S a )) = checkEven a

divNat :: Nat -> Nat -> Nat
divNat a b | compareNat a b == 0 = S Z
           | compareNat a b == -1 = Z
           | otherwise = S (divNat (subNat a b) b)

modNat :: Nat -> Nat -> Nat
modNat a b = subNat a (mulNat b (divNat a b))

