--1. Gere os primeiros n números ímpares a partir de 1.
--Exemplo:
--primeirosImpares 6 → [1,3,5,7,9,11]
--primeirosImpares 1 → [1]
primeirosImpares :: Int -> [Int]
primeirosImpares n = [2*x + 1 | x <- [0..n-1]]

--2. Crie uma sequência infinita de um caractere c e devolva os primeiros n caracteres.
--Exemplo:
--repeteChar 6 'x' → "xxxxxx"
---repeteChar 3 'A' → "AAA"
repeteChar :: Int -> Char -> [Char]
repeteChar x y = take x (repeat y)  

--3. Defina a função oDoMeio3 que, dados 3 inteiros, devolve o inteiro do meio (ou seja, a mediana).
--Exemplo:
--oDoMeio3 4 2 7 = 4
--oDoMeio3 2 (-1) 10 = 2
oDoMeio3 :: Int -> Int -> Int -> Int
minVal :: Int -> Int -> Int -> Int
maxVal :: Int -> Int -> Int -> Int
minVal x y z =  minimum [x,y,z]
maxVal x y z = maximum [x,y,z]
oDoMeio3 x y z = head [n | n <- [x,y,z], n /= minVal x y z, n /= maxVal x y z]


--4. Defina a função quantosAcimaDaMedia que, dados 3 inteiros, retorna o
--número de inteiros acima da média.
--Exemplo:
--quantosAcimaDaMedia 1 2 3 = 1
--quantosAcimaDaMedia 4 9 12 = 2
quantosAcimaDaMedia :: Int -> Int -> Int -> Int
media :: Int -> Int -> Int -> Int
media x y z =  (x + y + z) `div` 3
quantosAcimaDaMedia x y z = length [n | n <- [x,y,z], n > media x y z]

--5. Defina a função numero2 que, dados dois caracteres, retorna um inteiro em
--que o algarismo das dezenas é o primeiro carácter e o das unidades é o
--segundo, ou retorna zero se algum dos caracteres não for um dígito.
--Exemplo:
--numero2 '2' '3' = 23
--numero2 '4' '1' = 41
--numero2 :: Int
numero2 :: Char -> Char -> Int
numero2 x y
    | x `elem` ['0'..'9'] && y `elem` ['0'..'9'] = read [x, y]
    | otherwise = 0

--6. Defina a função anoBissexto que, dado um ano (um inteiro), devolve True se
--o ano é bissexto, False se não. (Nota: um ano bissexto é divisível por 400 ou
--é divisível por 4 mas não é divisível por 100)
anoBissexto :: Int -> Bool
anoBissexto x 
    | mod x 400 == 0  && mod x 100 /= 0 = True
    | mod x 4 == 0  && mod x 100 /= 0 = True
    | otherwise = False

--7. Crie uma função que divida uma lista ao meio e devolva as duas partes (como
--duas listas dentro de uma lista). Caso o número de elementos seja ímpar, a
--primeira parte deve ter um a mais.
--Exemplo:
--metade [1,2,3,4] → [[1,2],[3,4]]
--metade [10,20,30,40,50] → [[10,20,30],[40,50]]
quant :: [Int] -> Int
quant lista = length lista
metade :: [Int] -> [[Int]]
metade lista 
    | mod (quant lista) 2 == 0 = [take (quant lista `div` 2) lista, drop (quant lista `div` 2) lista]
    | otherwise = [take ((quant lista `div` 2) + 1) lista, drop ((quant lista `div` 2) + 1) lista]

--8. Crie uma função que elimine o primeiro e o último elemento de uma lista.
--Se tiver menos de dois elementos, devolva uma lista vazia.
--Exemplo:
--semExtremos [1,2,3,4,5] → [2,3,4]
--semExtremos [9] → []
semExtremos :: [Int] -> [Int]
semExtremos lista
    | length lista < 2 = []
    | otherwise = init (tail lista)

--9. Dadas duas extremidades, produza a lista com todos os valores entre elas,
--incluindo as extremidades.
--Exemplo:
--intervalo 5 9 → [5,6,7,8,9]
--intervalo 9 5 → [9,8,7,6,5]
intervalo :: Int -> Int -> [Int]
intervalo x y
    | x <= y = enumFromTo x y --[x..y]
    | otherwise = [x,x-1..y]

--10.Dada uma lista, divida-a ao meio e inverta a ordem das duas metades.
--Se o número de elementos for ímpar, o elemento central fica na segunda
--metade.
--Exemplo:
--inverteMetades [1,2,3,4] → [3,4,1,2]
--inverteMetades [10,20,30,40,50] → [30,40,50,10,20]
quantMet :: [Int] -> Int
quantMet lista = length lista
metadeLista :: [Int] -> [Int]
metadeLista lista  = drop (quant lista `div` 2) lista ++ take (quant lista `div` 2) lista


--11. Defina uma função que insere um elemento em uma posição dada,
--deslocando os demais à direita. Se a posição for maior que o tamanho, insira
--no final.
--Exemplo:
--inserePos 2 99 [10,20,30,40] → [10,20,99,30,40]
--inserePos 10 5 [1,2,3] → [1,2,3,5]
inserePos :: Int -> Int -> [Int] -> [Int]
inserePos pos elemento lista = take pos lista ++ [elemento] ++ drop pos lista

--12.Defina uma função que dado um inteiro troca o algarismo das unidades com o
--das dezenas.
--Exemplo:
--trocaPosicao 123 = 132
--trocaPosicao :: Int -> Int
trocaPosicao :: Int -> Int
trocaPosicao x = resto * 100 + u * 10 + d
  where
    u = x `mod` 10
    d = (x `div` 10) `mod` 10
    resto = x `div` 100

-- 13 -Escreva, em Haskell, a função invertInt::Int->Int que inverta os dígitos de um
--número inteiro.
--Exemplo: invertInt 123 = 321.
invertInt :: Int -> Int
invertInt x = read (reverse (show x)) :: Int


--14- Crie uma função que determine se um número é palíndromo (igual quando invertido).
--Exemplo:
--palindromo 121 → True
--palindromo 123 → False
--palindromo 1001 → True
palindromo :: Int -> Bool
palindromo n = n == (read (reverse (show n)) :: Int)

--15.Defina a função dezenas que, dado um inteiro, retorna o dígito das dezenas.
--Exemplo:
--dezenas 1234 → 3
--dezenas 507 → 0
--dezenas 9 → 0
dezenas :: Int -> Int
dezenas x = (x `div` 10) `mod` 10

--16 é igual a 12
