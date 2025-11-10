soma :: Int -> Int
soma 0 = 0
soma n = n + soma (n-1) --soma todos os valores de 0 até n

tamanhoLista :: [Int] -> Int 
tamanhoLista [] = 0
tamanhoLista (x:xs) = 1 + tamanhoLista xs

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) =  x + soma xs