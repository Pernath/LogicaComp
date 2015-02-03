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

power :: Nat -> Nat -> Nat
power Cero Cero = Suc Cero
power Cero (Suc n) = Cero
power (Suc n) Cero = Suc Cero 
power (Suc n) (Suc Cero) = Suc n
power (Suc Cero) (Suc n) = Suc Cero
-- a^b = a*[a^(b-1)]
power (Suc n) (Suc m) = prod (Suc n) (power (Suc n) m)








