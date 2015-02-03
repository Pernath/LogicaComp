--Datos Personales

--Tipo de dato para representar los naturales
data Nat = Cero | Suc Nat deriving Show

--Función recursiva auxiliar que nos permite conocer el sucesor de cualquier natural (Nat)
sucesor :: Nat -> Nat
sucesor Cero = Suc Cero
sucesor (Suc Cero) = Suc (Suc Cero)
sucesor (Suc n) = Suc (sucesor n)

--Función recursiva que nos permite obtener la suma de dos entradas de tipo Nat
suma :: Nat -> Nat -> Nat
suma Cero Cero = Cero
--
suma Cero (Suc n) = Suc n
suma (Suc n) Cero = Suc n
-- 
--suma (Suc Cero) (Suc n) = (sucesor(Suc n))
--suma (Suc n) (Suc Cero) = (sucesor(Suc n))
--
suma (Suc n) (Suc m) = Suc (Suc (suma n m))


doble :: Nat -> Nat
doble Cero = Cero
doble (Suc Cero) = Suc (Suc Cero)
doble (Suc n) = Suc (Suc (doble n))

--Función recursiva que nos permite obtener el producto de dos entradas de tipo Nat
prod :: Nat -> Nat -> Nat
prod Cero Cero = Cero
--
prod Cero (Suc n) = Cero
prod (Suc n) Cero = Cero
--
--prod (Suc Cero) (Suc n) = Suc n
--prod (Suc n) (Suc Cero) = Suc n
--
-- a*b = a + a*(b-1)
prod (Suc n) (Suc m) = suma (Suc n) (prod (Suc n) m)

mayorQue :: Nat -> Nat -> Bool
mayorQue Cero (Suc n) = False
mayorQue (Suc n) Cero = True
mayorQue (Suc n) (Suc m) = mayorQue n m

menorQue :: Nat -> Nat -> Bool
menorQue Cero (Suc n) = True
menorQue (Suc n) Cero = False
menorQue (Suc n) (Suc m) = menorQue n m

igual :: Nat -> Nat -> Bool
igual Cero Cero = True
igual Cero (Suc n) = False
igual (Suc n) Cero = False
igual (Suc n) (Suc m) = igual n m

powerNat :: Nat -> Nat -> Nat
powerNat Cero Cero = Suc Cero
powerNat Cero (Suc n) = Cero
powerNat (Suc n) Cero = Suc Cero 
powerNat (Suc n) (Suc Cero) = Suc n
powerNat (Suc Cero) (Suc n) = Suc Cero
-- a^b = a*[a^(b-1)]
powerNat (Suc n) (Suc m) = prod (Suc n) (powerNat (Suc n) m)

--aux 
esParNat :: Nat -> Bool
esParNat Cero = True
esParNat (Suc Cero) = False
esParNat (Suc (Suc Cero)) = True
esParNat (Suc (Suc n)) = esParNat n

power :: Int -> Int -> Int
power 0 0 = 1
power 0 i = 0
power i 0 = 1
power i 1 = i
power 1 i = 1
-- a^b = a*a^(b-1)
power i j = i*(power i (j-1))

esPar :: Int -> Bool
esPar 0 = True
esPar 1 = False
esPar 2 = True
esPar i = esPar (i-2)

power2 :: Int -> Int -> Int 
power2 x k
	| esPar x = power (power x 2) (div k 2) 
	| otherwise = x*(power x (k-1))

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]













