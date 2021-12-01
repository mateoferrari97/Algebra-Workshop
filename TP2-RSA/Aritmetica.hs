module Aritmetica where
import Tipos
import Data.Tuple
import Data.Bits

--(1)
mcdExt :: Integer -> Integer -> (Integer, (Integer, Integer))
mcdExt _ _ = (0, (0, 0))

--(2)
eliminarDivisoresDeN :: Set Integer -> Integer -> Set Integer
eliminarDivisoresDeN [] _ = []
eliminarDivisoresDeN (x:xs) n | x `mod` n == 0 = eliminarDivisoresDeN xs n
                              | otherwise = x : eliminarDivisoresDeN xs n

encontrarPrimosHastaN :: Set Integer -> Set Integer
encontrarPrimosHastaN [] = []
encontrarPrimosHastaN (x:xs) = x : encontrarPrimosHastaN nxs
  where nxs = eliminarDivisoresDeN xs x


criba :: Integer -> Set Integer
criba n | n <= 2 = []
        | otherwise = encontrarPrimosHastaN [2..n-1]

--(3)
maximoComunDivisor :: Integer -> Integer -> Integer
maximoComunDivisor x y | y == 0 = x
				               | otherwise = maximoComunDivisor y (x `mod` y)

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos x y = maximoComunDivisor x y == 1

encontrarPrimerCoprimoHastaN :: Integer -> Integer -> Integer
encontrarPrimerCoprimoHastaN x n | x >= (n-1) = 0
                                 | sonCoprimos x n = x
                                 | otherwise = encontrarPrimerCoprimoHastaN (x+1) n

coprimoCon:: Integer -> Integer
coprimoCon n | n <= 2 = 0
             | otherwise = encontrarPrimerCoprimoHastaN 2 n

--(4)
inversoMultiplicativo:: Integer -> Integer -> Integer
inversoMultiplicativo _ _ = 0



-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1
