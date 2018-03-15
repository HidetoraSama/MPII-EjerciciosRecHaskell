module Utilidades where

--------------------------------Utilidades de repositorio apdaza-------------------------------

agregarElemento::a->[a]->[a]
agregarElemento n (x:xs) = (n:x:xs)

encolarElemento::a->[a]->[a]
encolarElemento n [] = [n]
encolarElemento n (x:xs) = agregarElemento x (encolarElemento n (xs))

unirListas::[a]->[a]->[a]
unirListas (x:xs) (b:bs) = (x:xs) ++ (b:bs)

sumaLista::[Int]->Int
sumaLista [] = 0
sumaLista (b:bs) = b + sumaLista (bs)

doblarLista::[Int]->[Int]
doblarLista (x:xs) = [2*x | x <- (x:xs)]

cuentaApariciones::[Int]->Int->Int
cuentaApariciones [] x = 0
cuentaApariciones (b:bs) x = if b == x then 1 + cuentaApariciones (bs) x
                             else cuentaApariciones (bs) x

-------------------------------------Otras funciones utiles------------------------------------

primerDigito::Int->Int
primerDigito n = ultimoDigito (invertir n)

ultimoDigito::Int->Int
ultimoDigito n = if n < 10 then n
                 else n - (n `div` 10) * 10

removerExtremos::Int->Int
removerExtremos n
   | longitud n <= 2 = n
   | otherwise = (n `div` 10) - primerDigito n * 10 ^ (longitud n - 2)

enuplarLista::Int->[Int]->[Int]
enuplarLista n (x:xs) = [n*x | x <- (x:xs)]

esPar::Int->Bool
esPar n = n `mod` 2 == 0

esImpar::Int->Bool
esImpar n = n `mod` 2 == 1

tamLista::[a]->Int
tamLista [] = 0
tamLista (_:xs) = 1 + tamLista xs

substring::String->String->Bool
substring (x:xs) [] = False
substring xs ys
   | prefix xs ys = True
   | substring xs (tail ys) = True
   | otherwise = False

prefix::String->String->Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

-----------------------------Funciones recursividad primera entrega----------------------------

division::Int->Int->Int
division n m = if n < m then 0
               else 1 + division (n - m) m

fibonacci::Int->Int
fibonacci n
   | n == 0 = 0
   | n == 1 = 1
   | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

invertir::Int->Int
invertir n = if n < 10 then n
             else ultimoDigito n * 10 ^ longitud (n `div` 10) + invertir (n `div` 10)

longitud::Int->Int
longitud n = if n < 10 then 1
             else 1 + longitud (n `div` 10)

mayorDigito::Int->Int
mayorDigito n
   | n < 10 = n
   | n `mod` 10 > mayorDigito (n `div` 10) = n `mod` 10
   | otherwise = mayorDigito (n `div` 10)

palindromo::Int->Bool
palindromo n
   | longitud n <= 1 = True
   | primerDigito n == ultimoDigito n = palindromo (removerExtremos n)
   | otherwise = False

potencia::Int->Int->Int
potencia m n
   | n == 0 = 1
   | n == 1 = m
   | otherwise = m * potencia m (n - 1)

producto::Int->Int->Int
producto m n
   | m == 0 = 0
   | m == 1 = n
   | otherwise = n + producto n (m - 1)

sumaDigitos::Int->Int
sumaDigitos n = if n < 10 then n
                else n `mod` 10 + sumaDigitos (n `div` 10)
-----------------------------------Funciones solicitadas---------------------------------------

invertirLista::[a]->[a]
invertirLista [] = []
invertirLista (x:xs) = invertirLista xs ++ [x]

sumaPares::[Int]->Int
sumaPares [] = 0
sumaPares (x:xs) = sumaLista ([x | x <- (x:xs), esPar(x)])

sumaParesRec::[Int]->Int
sumaParesRec [] = 0
sumaParesRec (b:bs)
   | esPar(b) = b + sumaParesRec (bs)
   | otherwise = sumaParesRec (bs)

cantidadImpares::[Int]->Int
cantidadImpares [] = 0
cantidadImpares (x:xs) = tamLista ([x | x <- (x:xs), esImpar(x)])

cantidadImparesRec::[Int]->Int
cantidadImparesRec [] = 0
cantidadImparesRec (b:bs)
   | esImpar(b) = 1 + cantidadImparesRec (bs)
   | otherwise = cantidadImparesRec (bs)

contieneLista::[a]->[a]
contieneLista [] = []
--contieneLista (x:xs) = [typeOf x | x <- (x:xs)]

maximo::[Int]->Int
maximo [x] = x
maximo (x:n:xs)
   | x >= n = maximo (x:xs)
   | otherwise = maximo (n:xs)