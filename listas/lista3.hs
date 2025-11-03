--Crie uma função que inverta os elementos de uma tupla 
--Exemplo:
--troca (1,2) → (2,1)
troca :: (x,y) -> (y,x)
troca (x, y) = (y,x)

---Crie uma função que recebe duas tuplas, cada uma contendo o nome de uma
--disciplina e a nota do aluno nessa disciplina. A função deve retornar:
--a. "materias diferentes" se os nomes das disciplinas não forem iguais;
--b. "Aprovado" se as matérias forem iguais e a média aritmética das duas
--notas for maior ou igual a 6;
--c. "Reprovado" caso contrário.
media :: (String, Float) -> (String, Float) -> Float
media (m1, n1) (m2, n2) = (n1+n2)/2
materias :: (String, Float) -> (String, Float) -> String
materias (m1, n1) (m2, n2)
    | m1 /= m2  = "materias diferentes"
    | media (m1, n1) (m2, n2) >= 6.0 = "Aprovado"
    | otherwise = "Reprovado"
    
--Dada uma lista de tuplas [(Nome, Idade)], defina a função maioresDeIdade
--que retorna uma lista com os nomes (somente as strings) das pessoas com
--18 anos ou mais.
--Exemplo:
--maioresDeIdade [("Ana", 17), ("Beto", 20)] => ["Beto"]
maioresDeIdade :: [(String, Int)] -> [String]
maioresDeIdade pessoas = [ nome | (nome, idade) <- pessoas, idade >= 18]

--Crie uma função que recebe duas horas (hora, minuto) e devolve a diferença
--entre elas.
--Exemplo:
--diferencaHoras (14, 30) (12, 45) -> (1,45)
diferencaHoras :: (Int, Int) -> (Int, Int) -> (Int, Int)
diferencaHoras (h1, m1) (h2, m2) = (abs (h1 * 60 + m1 - h2 * 60 - m2) `div` 60, abs (h1 * 60 + m1 - h2 * 60 - m2) `mod` 60)

--Defina a função primos n que retorna todos os números primos até utilizando
--a lógica da Compreensão de Listas, onde um número é primo se não possui
--divisores no intervalo.
--Exemplo:
--primos 10 => [2, 3, 5, 7]
primos :: Int -> [Int]
primos n = [x | x <- [0..n], mod x n == 0]

--6 é igual a 4

--Dada uma lista de (cidade, temperatura), filtre as que estão acima de 30°C.
--Exemplo:
--temperaturasAltas [("Alfenas", 38), ("Alterosa", 5), ("Areado", 40)]->[("Alfenas", 38),("Areado", 40)]
temp :: [(String, Int)] ->[(String, Int)]
temp temps = [(x,y) | (x,y) <- temps, y>30]

--Utilizando listas em compreensão, defina a função termina_em que recebe
--um número n e devolve a lista com os números entre 0 e 100 que terminam
--com o número n.
--Exemplo:
--termina_em 3 = [3,13,23,33,43,53,63,73,83,93]
termina_em :: Int -> [Int]
termina_em num = [x | x <- [0..100], mod x 10 == num]

--Defina a função divisivelPor que dado um inteiro (de 0 a 9) retorna uma lista
--com todos os números entre 1 e 100 que são divisíveis pelo número.
--Exemplo:
--divisivel 7 =[7,14,21,28,35,42,49,56,63,70,77,84,91,98]
divisivelPor :: Int -> [Int]
divisivelPor num = [x | x <- [1..100], mod x num == 0]

--Sem usar a função length, defina a função comprimento que dada uma lista
--retorna o número de elementos da lista.
--Exemplo:
--comprimento [1,2,1,3] = 4
comprimento :: [Int] -> Int
comprimento x = sum [1 | _ <- x]

--Defina a função quantos que dada uma lista e um elemento devolve o número
--de vezes que o elemento está na lista.
--Exemplo:
--quantos [3,7,4,3,9,3,0,3,2] 3 = 4
quantos :: [Int] -> Int -> Int
quantos lista num = sum [1 | x <- lista, x == num]

--Defina a função repetidos que dada uma lista retorna uma lista com os
--elementos que estão repetidos.
--Exemplo:
--repetidos [1,2,1,3,4,3,1] = [1,3]
conta :: (Eq a) => a -> [a] -> Int
conta x lista = length [y | y <- lista, y == x]

removeDuplicados :: (Eq a) => [a] -> [a]
removeDuplicados [] = []
removeDuplicados (x:xs)
    | x `elem` xs = removeDuplicados xs
    | otherwise   = x : removeDuplicados xs

repetidos :: (Eq a) => [a] -> [a]
repetidos xs = removeDuplicados [x | x <- xs, conta x xs > 1]

--Defina a função contaQuantosDiferentes que retornam o número de
--elementos diferentes do inteiro na lista.
--Exemplo:
--contaQuantosDiferentes [1,3,4,1] 1 = 2
contaQuantosDiferentes :: [Int] -> Int -> Int
contaQuantosDiferentes lista num = sum [1 | x <- lista, x /= num]
