--Implemente uma função meuAbs que devolve o valor absoluto sem usar abs.
--Exemplo:
--meuAbs (-5) → 5
--meuAbs 7 → 7
meuAbs x 
    | x>0 = x
    | x<0 = x * (-1)

--Crie uma função que receba dois números e devolva o maior deles.
--Exemplo:
--maior2 10 7 → 10
--maior2 (-3) 5 → 5
maior :: Int -> Int -> Int
maior x y 
    | x>y = x 
    | x<y = y 

--Escreva uma função que diga se um número é positivo, negativo ou zero.
--Exemplo:
--classificaSinal (-8) → "Negativo"
--classificaSinal 0 → "Zero"
--classificaSinal 42 → "Positivo"
classificaSinal :: Int -> String
classificaSinal x 
    | x>0 = "Sinal positivo"
    | x<0 = "Sinal negativo"
    | x==0 = "Zero"

--Crie uma função que classifique uma nota (0 a 10) em conceitos: Excelente,
--Bom, Regular, Insuficiente.
--Exemplo:
--conceito 9.5 → "Excelente"
--conceito 7.0 → "Bom"
--conceito 5.5 → "Regular"
--conceito 3.0 → "Insuficiente"
classificaNota :: Float -> String
classificaNota x 
    | x >= 0.0 && x < 4.0  = "Insuficiente"
    | x >= 4.0 && x < 6.0  = "Regular"
    | x >= 6.0 && x < 8.0  = "Bom"
    | x >= 8.0 && x <= 10.0 = "Excelente"
    | otherwise = "Nota inválida"

--Escreva funções que retornem o maior e o menor de tre̊s números.
--Exemplo:
--maior3 4 9 2 → 9
--menor3 4 9 2 → 2
maior3 :: Int -> Int -> Int -> Int
maior3 x y z
    | x >= y && x >= z = x
    | y >= x && y >= z = y
    | otherwise = z
menor3 :: Int -> Int -> Int -> Int
menor3 x y z
    | x <= y && x <= z = x
    | y <= x && y <= z = y
    | otherwise = z

--Crie uma calculadora simples que receba dois números e uma operação ('+','-', '', '/').
--Exemplo:
--calc 8 3 '+' → 11
--calc 8 3 '-' → 5
--calc 8 3 '' → 24
--calc 8 2 '/' → 4
calc :: Float -> Float -> Char -> Float
calc x y z 
    | z == '+' = x+y
    | z == '-' = x-y
    | z == '*' = x*y
    | z == '/' = x / y

--Faça uma função que classifique um triângulo como Equilátero, Isósceles ou Escaleno.
--Exemplo:
--tipoTriangulo 3 3 3 → "Equilátero"
--tipoTriangulo 3 3 2 → "Isósceles"
--tipoTriangulo 3 4 5 → "Escaleno"
tipoTriangulo :: Int -> Int -> Int -> String
tipoTriangulo x y z
    | x == y && x == z = "Equilátero"
    | x == y && x /= z = "Isósceles"
    | x == z && x /= y = "Isósceles"
    | y == z && y /= x = "Isósceles"
    | otherwise = "Escaleno"

--Crie uma função que calcule a diferença absoluta entre dois números.
--Exemplo:
--difAbs 10 3 → 7
--difAbs 3 10 → 7
difAbs :: Int -> Int -> Int
difAbs x y = abs(x-y)

--Escreva uma função que diga se um número é múltiplo de outro.
--Exemplo:
--multiploDe 15 5 → True
--multiploDe 14 5 → False
multiploDe :: Int -> Int -> String
multiploDe x y 
    | mod x y == 0 = "True"
    | mod x y /= 0 = "False"

--Defina a função min3 que, dados 3 inteiros, retorna o menor deles.
--Exemplo:
--min3 7 2 9 → 2
--min3 (-1) 0 (-5) → -5
min3 :: Int -> Int -> Int -> Int
min3 x y z
    | x <= y && x <= z = x
    | y <= x && y <= z = y
    | otherwise = z

--Defina a função dezenas que dado um inteiro toma o valor do dígito das dezenas.
--Ex: dezenas 1234 é 3
dezenas :: Int -> Int
dezenas x = (x `div` 10) `mod` 10

--Defina a função centenas que dado um inteiro toma o valor do dígito das centenas.
--ex: centenas 1234 é 2
centenas :: Int -> Int
centenas x = (x `div` 100) `mod` 10