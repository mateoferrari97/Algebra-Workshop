-- Funcion de prueba de Scope
integrantes :: String
integrantes = "Fernandez Ledo, Emilio - Ferrari Coronel, Mateo - Magatelli, Axel Einar"

{-  Estrategia empleada, parecida al ejercicio del triángulo de Pascal que vamos a denominar "Triangulo de Ramanujan"
    de la Clase 03, para localizar rápidamente los numeros que cumplen ser la suma de dos números al cubo.

    La tabla ilustra todos los números formados por la suma de dós números al cubo. Aquellos que
    tienen al menos dos soluciones, aparecen repetidos.

    Por ejemplo el número 1729 aparece en la fila 10_9 y 12_1, lo que coincide con las
    dos soluciones (10^3 + 9^3 = 12^3 + 1^3 = 1729).

              | Columna _____________________________________________________________
         _____|__ 1 __ 2 __ 3 __ 4 __ 5 __ 6 __ 7 __ 8 __ 9 _ 10 _ 11 _ 12 _ 13 _ 14
    Fila |  1 |    2   |    |    |    |    |    |    |    |    |    |    |    |    |      
         |  2 |    9   16   |    |    |    |    |    |    |    |    |    |    |    |      
         |  3 |   28   35   54   |    |    |    |    |    |    |    |    |    |    |      
         |  4 |   65   72   91  128   |    |    |    |    |    |    |    |    |    |        
         |  5 |  126  133  152  189  250   |    |    |    |    |    |    |    |    |        
         |  6 |  217  224  243  280  341  432   |    |    |    |    |    |    |    |      
         |  7 |  344  351  380  407  468  559  686   |    |    |    |    |    |    |      
         |  8 |  513  520  539  576  637  728  855 1024   |    |    |    |    |    |      
         |  9 |  730  737  756  793  854  945 1072 1241 1458   |    |    |    |    |         
         | 10 | 1001 1008 1027 1064 1125 1216 1343 1512 1729 2000   |    |    |    |           
         | 11 | 1332 1339 1358 1395 1456 1547 1674 1843 2060 2331 2662   |    |    |       
         | 12 | 1729 1736 1755 1792 1853 1944 2071 2240 2457 2728 3059 3456   |    |    
         | 13 | 2198 2205 2224 2261 2322 2413 2540 2709 2926 3197 3528 3925 4394   | 
         | 14 | 2745 2752 2771 2808 2869 2960 3087 3256 3473 3744 4075 4472 4941 5488

    Referencia: http://www.durangobill.com/Ramanujan.html - Ramanujan Numbers and The Taxicab Problem
-}

-- Ejercicio 1 -----------------------------------------------------------------------------------------------------------------------------------------------

sumaDeDosCubosRecorrerFilas :: Integer -> Integer -> Bool
sumaDeDosCubosRecorrerFilas fila n | (primerElementoDeLaFila fila) > n = False -- busca otras posibilidades de filas de no encontrar tu numero mirando el primer valor de la fila nueva
                                   | primerElemento /= -1 && segundoElemento /= -1 = True -- compara con la linea de codigo 50
                                   | otherwise = sumaDeDosCubosRecorrerFilas (fila+1) n
    where resultado = recorrerFilaHastaEncontrarN fila 1 fila n
          primerElemento = fst resultado
          segundoElemento = snd resultado
-- suma los extremos y saca el medio (busqueda binaria). Si no encuentra, vuelve a hacer
-- desde hasta en el nuevo pedazo de celdas. condicion de corte es cuando el desde es > a hasta
-- y devuelve -1, lo que hace que salte a la siguiente fila y de no encontrar el numero, corta.

sumaDeDosCubosRecorrerMatrizHastaEncontrarN :: Integer -> Bool
sumaDeDosCubosRecorrerMatrizHastaEncontrarN n = sumaDeDosCubosRecorrerFilas fila n
    where fila = encontrarFilaCandidata n

esSumaDeDosCubos :: Integer -> Bool
esSumaDeDosCubos n = sumaDeDosCubosRecorrerMatrizHastaEncontrarN n





-- Ejercicio 2 -----------------------------------------------------------------------------------------------------------------------------------------------
{--
descomposicionCubosRecorrerFilasColumnas :: Integer -> Integer -> Integer -> (Integer, Integer)
descomposicionCubosRecorrerFilasColumnas fila col n | col > fila = (0, 0)
                                                    | col == 1 && resultado > n = (-1, -1)
                                                    | resultado == n = (fila, col)
                                                    | otherwise = descomposicionCubosRecorrerFilasColumnas fila (col+1) n
    where resultado = fila^3 + col^3
--}

descomposicionCubosRecorrerFilas :: Integer -> Integer -> (Integer, Integer)
descomposicionCubosRecorrerFilas fila n | (primerElementoDeLaFila fila) > n = (-1, -1)
                                        | primerElemento /= -1 && segundoElemento /= -1 = resultado
                                        | otherwise = descomposicionCubosRecorrerFilas (fila+1) n
    where resultado = recorrerFilaHastaEncontrarN fila 1 fila n
          primerElemento = fst resultado
          segundoElemento = snd resultado

descomposicionCubosRecorrerMatrizHastaEncontrarN :: Integer -> (Integer, Integer)
descomposicionCubosRecorrerMatrizHastaEncontrarN n = descomposicionCubosRecorrerFilas fila n -- compongo la tupla de la suma de cubos
    where fila = encontrarFilaCandidata n

descomposicionCubos :: Integer -> (Integer, Integer)
descomposicionCubos n = descomposicionCubosRecorrerMatrizHastaEncontrarN n





-- Ejercicio 3 -----------------------------------------------------------------------------------------------------------------------------------------------

cantidadDeFormasRecorrerFilas :: Integer -> Integer -> Integer
cantidadDeFormasRecorrerFilas fila n | (primerElementoDeLaFila fila) > n = 0
                                     | primerElemento /= -1 && segundoElemento /= -1 = 1 + cantidadDeFormasRecorrerFilas (fila+1) n -- como ya encontre4 una solucion, buscame otro por las dudas
                                     | otherwise = cantidadDeFormasRecorrerFilas (fila+1) n
    where resultado = recorrerFilaHastaEncontrarN fila 1 fila n
          primerElemento = fst resultado
          segundoElemento = snd resultado

cantidadDeFormasRecorrerMatrizHastaEncontrarN :: Integer -> Integer
cantidadDeFormasRecorrerMatrizHastaEncontrarN n = cantidadDeFormasRecorrerFilas fila n
    where fila = encontrarFilaCandidata n

cantidadDeFormas :: Integer -> Integer
cantidadDeFormas n = cantidadDeFormasRecorrerMatrizHastaEncontrarN n





-- Ejercicio 4 -----------------------------------------------------------------------------------------------------------------------------------------------
    {-  En este caso, es necesario optimizar las busquedas ya que en numeros de muchas cifras, la cardinalidad de la fila aumenta considerablemente.
        Por ello la funcion columnaCandidata permite determinar una columna en cual arrancar evaluando un valor que tenga al menos 2 soluciones y que
        sea mas grande o igual al valor buscado.
    -}

columnaCandidata :: Integer -> Integer -> Integer -> Integer -> Integer
columnaCandidata fila desde hasta n | desde > hasta = -1
                                    | candidata == n = medio
                                    | candidata > n && candidataAnterior < n = medio-1 -- Busca desde 1 mas chico que el numero que te pasaron
                                    | candidata > n && candidataAnterior > n = columnaCandidata fila desde (hasta-1) n
                                    | otherwise = columnaCandidata fila (desde+1) hasta n
    where medio = div (desde+hasta) 2
          candidata = fila^3 + medio^3
          candidataAnterior = fila^3 + (medio-1)^3 -- candidatoAnerior busca un numero que sea mayor o igla al numero pasado

encontrarColumnaCandidata :: Integer -> Integer -> Integer
encontrarColumnaCandidata fila n = columnaCandidata fila 1 fila n

numeroEspecialMayorIgualN :: Integer -> Integer -> Integer -> Integer
numeroEspecialMayorIgualN fila col n | col > fila = -1
                                     | resultado >= n && cantidadDeFormas resultado >= 2 = resultado
                                     | otherwise = numeroEspecialMayorIgualN fila (col+1) n
    where resultado = fila^3 + col^3


especialDesdeRecorrerFilas :: Integer -> Integer -> Integer -> Integer
especialDesdeRecorrerFilas fila n minimo | (primerElementoDeLaFila fila) >= minimo && n <= minimo = minimo -- ojo que el primer numero que encuentre en las filas, no es el minimo inmediato por lo cual tengo q buscarlo
                                         | resultado /= -1 && minimo == 1729 = especialDesdeRecorrerFilas (fila+1) n resultado
                                         | resultado /= -1 && resultado < minimo = especialDesdeRecorrerFilas (fila+1) n resultado  -- esta linea es la que corta al encontrer el especial mas chico
                                         | otherwise = especialDesdeRecorrerFilas (fila+1) n minimo
    where col = encontrarColumnaCandidata fila n -- con esto, busco una columna candidata para no evaluar al pedo los valores anteriores
          resultado = numeroEspecialMayorIgualN fila col n



especialDesdeRecorrerMatrizHastaEncontrarN :: Integer -> Integer
especialDesdeRecorrerMatrizHastaEncontrarN n | n <= 1729 = 1729
                                             | otherwise = especialDesdeRecorrerFilas fila n 1729
    where fila = encontrarFilaCandidata n -- esta es la fila de siempre y desde esta fila en adelante

especialDesde :: Integer -> Integer
especialDesde n = especialDesdeRecorrerMatrizHastaEncontrarN n





-- Ejercicio 5 -----------------------------------------------------------------------------------------------------------------------------------------------

especialNumeroRecorrerFilasColumnas :: Integer -> Integer -> Integer -> Integer
especialNumeroRecorrerFilasColumnas fila col anterior | col > fila = 0
                                                      | resultado /= anterior && cantidadDeFormas resultado >= 2 = resultado -- verifica si no se repiten los numeros especiales encontrados y sigue de ser necesario
                                                      | otherwise = especialNumeroRecorrerFilasColumnas fila (col+1) anterior
    where resultado = fila^3 + col^3

especialNumeroRecorrerFilas :: Integer -> Integer -> Integer -> Integer -> Integer
especialNumeroRecorrerFilas fila tengo quiero anterior | resultado /= 0 && tengo == quiero = resultado
                                                       | resultado /= 0 && tengo /= quiero = especialNumeroRecorrerFilas (fila+1) (tengo+1) quiero resultado -- contador de especiales
                                                       | otherwise = especialNumeroRecorrerFilas (fila+1) tengo quiero anterior
    where resultado = especialNumeroRecorrerFilasColumnas fila 1 anterior

especialNumeroRecorrerMatrizHastaEncontrarN :: Integer -> Integer
especialNumeroRecorrerMatrizHastaEncontrarN n = especialNumeroRecorrerFilas 1 1 n 0 

especialNumero :: Integer -> Integer
especialNumero n = especialNumeroRecorrerMatrizHastaEncontrarN n





-- Ejercicio 6 -----------------------------------------------------------------------------------------------------------------------------------------------

esUnCubo :: Integer -> Bool
esUnCubo x = (round (fromIntegral x ** (1 / 3))) ^ 3 == x

conRestoCero :: Float -> Bool
conRestoCero n = n == fromInteger (round n)

cumpleSerMuyEspecial :: Integer -> Integer -> Bool
cumpleSerMuyEspecial nEspecial n | k <= 1 = True
                                 | k > 1 && conRestoCero k && esUnCubo (round k) = False
                                 | otherwise = cumpleSerMuyEspecial (nEspecial + 1) n
    where m = especialNumero nEspecial
          k = (fromInteger n / fromInteger m)

esMuyEspecial :: Integer -> Bool
esMuyEspecial n = cantidadDeFormas n >= 2 && cumpleSerMuyEspecial 1 n





-- Funciones auxiliares -----------------------------------------------------------------------------------------------------------------------------------------------

-- busca la fila que tiene el numero que nos intereza recorriedno las diagonales recursivamente hasta encontrar un nº q sea mayor o igual a n.
-- Formando asi la matriz nque necesitamos y descartando las filas anteriores.
filaCandidata :: Integer -> Integer -> Integer
filaCandidata fila n | resultado >= n = fila
                     | otherwise = filaCandidata (fila+1) n
    where resultado = fila^3 + fila^3

-- Con esta funcion, logramos dar con el numero de Ramanujan que nos interese analizar
encontrarFilaCandidata :: Integer -> Integer
encontrarFilaCandidata n = filaCandidata 1 n

-- busca dentro de la fila un rango de numeros y da como resultado las filas que son las sumas de nuestro valor buscado
    {-  Con esta funcion se busca optimizar las busquedas empleando la estrategia de "Divide y reinaras" (similar a la "Busqueda binaria") 
        vsistos en la teorica de la catedra y en el ejercico de la clase 5 de busqueda del menorFactorialDesde

        Referncia:  https://www.youtube.com/watch?v=kDeVHTWp2-g&t=1s - Teórica 7 1ra parte - Inducción completa
                    https://www.youtube.com/watch?v=g0PS6rz4pG0 - Taller de Algebra - Teorica Clase 5 
    -}
recorrerFilaHastaEncontrarN :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
recorrerFilaHastaEncontrarN fila desde hasta n | desde > hasta = (-1, -1)
                                        | resultado == n = (fila, medio)
                                        | resultado > n = recorrerFilaHastaEncontrarN fila desde (medio-1) n
                                        | otherwise = recorrerFilaHastaEncontrarN fila (medio+1) hasta n
    where medio = div (desde+hasta) 2 -- divide y reinaras como en la clase de haskell de buscar minimo comun multiplo
          resultado = fila^3 + medio^3
        -- divir y concquistar o busqueda binaria es un algo de buqeda en conjunto finito y ordenado!!

primerElementoDeLaFila :: Integer -> Integer
primerElementoDeLaFila fila = fila^3 + col^3
    where col = 1

tests = testsEsSumaDeDosCubos ++ " -- " ++ testsDescomposicionCubos ++ " -- " ++ testsCantidadDeFormas ++ " -- " ++ testsEspecialDesde ++ " -- " ++ testsEspecialNumero ++ " -- " ++ testsEsMuyEspecial

testsEsSumaDeDosCubos | esSumaDeDosCubos_caso_1 && esSumaDeDosCubos_caso_2 && esSumaDeDosCubos_caso_3 && esSumaDeDosCubos_caso_4 && esSumaDeDosCubos_caso_5 = "Ejercicio 1 - OK"
                      | otherwise = "Ejercicio 1 - ERROR"
    where esSumaDeDosCubos_caso_1 = esSumaDeDosCubos 88 == False
          esSumaDeDosCubos_caso_2 = esSumaDeDosCubos 2 == True
          esSumaDeDosCubos_caso_3 = esSumaDeDosCubos 79625 == True
          esSumaDeDosCubos_caso_4 = esSumaDeDosCubos 97187584969377 == True
          esSumaDeDosCubos_caso_5 = esSumaDeDosCubos 97187584969378 == False

testsDescomposicionCubos | descomposicionCubos_caso_1 && descomposicionCubos_caso_2 && descomposicionCubos_caso_3 && descomposicionCubos_caso_4  = "Ejercicio 2 - OK"
                         | otherwise = "Ejercicio 2 - ERROR"
    where descomposicionCubos_caso_1_resultado = descomposicionCubos 9
          descomposicionCubos_caso_1_fst = fst descomposicionCubos_caso_1_resultado
          descomposicionCubos_caso_1_snd = snd descomposicionCubos_caso_1_resultado
          descomposicionCubos_caso_1 = 9 == descomposicionCubos_caso_1_fst^3 + descomposicionCubos_caso_1_snd^3
          descomposicionCubos_caso_2_resultado = descomposicionCubos 1729
          descomposicionCubos_caso_2_fst = fst descomposicionCubos_caso_2_resultado
          descomposicionCubos_caso_2_snd = snd descomposicionCubos_caso_2_resultado
          descomposicionCubos_caso_2 = 1729 == descomposicionCubos_caso_2_fst^3 + descomposicionCubos_caso_2_snd^3
          descomposicionCubos_caso_3_resultado = descomposicionCubos 4104
          descomposicionCubos_caso_3_fst = fst descomposicionCubos_caso_3_resultado
          descomposicionCubos_caso_3_snd = snd descomposicionCubos_caso_3_resultado
          descomposicionCubos_caso_3 = 4104 == descomposicionCubos_caso_3_fst^3 + descomposicionCubos_caso_3_snd^3
          descomposicionCubos_caso_4_resultado = descomposicionCubos 20683
          descomposicionCubos_caso_4_fst = fst descomposicionCubos_caso_4_resultado
          descomposicionCubos_caso_4_snd = snd descomposicionCubos_caso_4_resultado
          descomposicionCubos_caso_4 = 20683 == descomposicionCubos_caso_4_fst^3 + descomposicionCubos_caso_4_snd^3

testsCantidadDeFormas | cantidadDeFormas_caso_1 && cantidadDeFormas_caso_2 && cantidadDeFormas_caso_3 && cantidadDeFormas_caso_4 = "Ejercicio 3 - OK"
                      | otherwise = "Ejercicio 3 - ERROR"
    where cantidadDeFormas_caso_1 = cantidadDeFormas 88 == 0
          cantidadDeFormas_caso_2 = cantidadDeFormas 4104 == 2
          cantidadDeFormas_caso_3 = cantidadDeFormas 6963472309248 == 4
          cantidadDeFormas_caso_4 = cantidadDeFormas 9 == 1

testsEspecialDesde | especialDesde_caso_1 && especialDesde_caso_2 && especialDesde_caso_3 && especialDesde_caso_4 && especialDesde_caso_5 = "Ejercicio 4 - OK"
                   | otherwise = "Ejercicio 4 - ERROR"
    where especialDesde_caso_1 = especialDesde 5 == 1729
          especialDesde_caso_2 = especialDesde 1729 == 1729
          especialDesde_caso_3 = especialDesde 1730 == 4104
          especialDesde_caso_4 = especialDesde 8000 == 13832
          especialDesde_caso_5 = especialDesde 87539299 == 87539319

testsEspecialNumero | especialNumero_caso_1 && especialNumero_caso_2 && especialNumero_caso_3 = "Ejercicio 5 - OK"
                   | otherwise = "Ejercicio 5 - ERROR"
    where especialNumero_caso_1 = especialNumero 1 == 1729
          especialNumero_caso_2 = especialNumero 4 == 20683
          especialNumero_caso_3 = especialNumero 12 == 110808

testsEsMuyEspecial | esMuyEspecial_caso_1 && esMuyEspecial_caso_2 && esMuyEspecial_caso_3 && esMuyEspecial_caso_4 && esMuyEspecial_caso_5 = "Ejercicio 6 - OK"
                   | otherwise = "Ejercicio 6 - ERROR"
    where esMuyEspecial_caso_1 = esMuyEspecial 165464 == False
          esMuyEspecial_caso_2 = esMuyEspecial 1729 == True
          esMuyEspecial_caso_3 = esMuyEspecial 3 == False
          esMuyEspecial_caso_4 = esMuyEspecial 20683 == True
          esMuyEspecial_caso_5 = esMuyEspecial 805688 == True


----------------------------------------------------------------------------------------------------------------------------------------------------------------
{- Bibliografia consultada:


Durango Bill’s - Ramanujan Numbers and The Taxicab Problem - Recuperado 2021/09/26
http://www.durangobill.com/Ramanujan.html

Kevin A. Broughan - University of Waikato - Hamilton 2001 - New Zealand - [Archivo PDF]
https://cs.uwaterloo.ca/journals/JIS/VOL6/Broughan/broughan25.pdf

Normas APA - Carlos Sanchez - Publicado 05/02/2020 - ¿Cómo citar una Página Web?
https://normas-apa.org/referencias/citar-pagina-web/

The OEIS Foundation - N. J. A. Sloane - Taxi-cab numbers: sums of 2 cubes in more than 1 way - Recuperado 2021/09/26
https://oeis.org/A001235

The OEIS Foundation - Stuart Gascoigne - Cubefree taxicab numbers: the smallest cubefree number that is the sum of 2 positive cubes in n ways - 2020/08/21
https://oeis.org/A080642

UBA - Álgebra I 1 C2021 - Teórica 7 1ra parte - Inducción completa - 13/04/2021 - 09min 15s - [Archivo de Vídeo]
https://www.youtube.com/watch?v=kDeVHTWp2-g&t=1s

UBA - Taller de Algebra I - Clase 5: Recursión con funciones auxiliares - 28/04/2020 - 1Hs 04min 25s - [Archivo de Vídeo]
https://www.youtube.com/watch?v=g0PS6rz4pG0

Wikipedia - Algoritmo divide y vencerás - 12/03/2019
https://es.wikipedia.org/wiki/Algoritmo_divide_y_vencer%C3%A1s

Wikipedia - Búsqueda binaria - 15/02/2021
https://es.wikipedia.org/wiki/B%C3%BAsqueda_binaria

-}

