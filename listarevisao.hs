-- 1- classificaLista :: [Int] -> String
--Retorna "Lista vazia", "Lista unitária" ou "Lista longa" conforme o tamanho.
--Exemplo:
--classificaLista [] → "Lista vazia"

classificaLista :: [Int] -> String
classificaLista lista
    | length lista == 0 = "Lista vazia"
    | length lista == 1 = "Lista unitária"
    | otherwise = "Lista longa"


--2- media3 :: (Float, Float, Float) -> Float. Retorna a média aritmética de três valores.
--Exemplo:
--media3 (4.0, 6.0, 10.0) → 6.666667

media3 :: (Float, Float, Float) -> Float
media3 (x,y,z) = (x+y+z)/3

--3 - acimaDaMedia :: [(String,Float)] -> [String]. Dada uma lista de alunos e notas, retorna
--os nomes com nota > média geral.
--Exemplo:
--acimaDaMedia [("Ana",8.0),("Beto",5.0),("Clara",7.0)] → ["Ana","Clara"]

aluno :: Int -> (String, Float)
aluno codigo
    | codigo == 1 = ("Ana", 8.0)
    | codigo == 2 = ("Beto", 5.0)
    | codigo == 3 = ("Clara", 7.0)
    | codigo == 4 = ("Daniel", 6.5)
    | codigo == 5 = ("Eduarda", 9.0)
    | codigo == 6 = ("Felipe", 4.0)
    | otherwise = ("Aluno não cadastrado", 11.0)

-- Função para criar lista de alunos
criarListaAlunos :: Int -> [(String, Float)]
criarListaAlunos nRegistros = [aluno x | x <- [1..nRegistros]]

-- Filtra apenas alunos cadastrados (exclui "Aluno não cadastrado")
alunosValidos :: Int -> [(String, Float)]
alunosValidos nRegistros = [(x, y) | (x, y) <- criarListaAlunos nRegistros, y < 11]

-- Calcula a média das notas
calcularMedia :: Int -> Float
calcularMedia nRegistros = sum [y | (_, y) <- alunosValidos nRegistros] / fromIntegral (length (alunosValidos nRegistros))

-- Função principal - retorna nomes com nota acima da média
acimaDaMedia :: Int -> [String]
acimaDaMedia nRegistros = [x | (x, y) <- alunosValidos nRegistros, y > calcularMedia nRegistros]

--4- diferencas :: [Int] -> [Int]
--Dada uma lista, retorna as diferenças entre elementos consecutivos.
--Exemplo:
--diferencas [10,7,4] → [-3,-3]
--diferencas [5,9,3,1] → [4,-6,-2]
diferencas :: [Int] -> [Int]
diferencas x = zipWith (-) (tail x) x

--5- Escreva uma função que retorna o dígito de um número inteiro de acordo com a
--posição informada.
--Exemplo:
--anyDigit 0 7689 = 7
--anyDigit 2 7689 = 8
--anyDigit 9 7689 = -1
transformaEmTexto :: Int -> String
transformaEmTexto numero = show numero

transformaEmNumero :: Char -> Int
transformaEmNumero char = read [char] :: Int

pegarCaractereNaPosicao :: Int -> Int -> Char
pegarCaractereNaPosicao posicao numero = last (take (posicao + 1) (transformaEmTexto numero))

pegarDigito :: Int -> Int -> Int
pegarDigito posicao numero
    | posicao < 0 || posicao > (length (transformaEmTexto numero) - 1) = -1
    | otherwise = transformaEmNumero (pegarCaractereNaPosicao posicao numero)

-- 6- Fazer a função
funcao :: Float -> Float 
funcao x 
    | x>0 || x==0 = (x+4)/(x+2)
    | otherwise = 2/x

--7- Faça uma solução para inverter os elementos de uma lista de Inteiros.
--Exemplo:
--inverte [1,2,3,4,5,6,150] = [150,6,5,4,3,2,1]
inverte :: [Int] -> [Int]
inverte lista = reverse lista

--8 - Implemente a função pushRight::String->Int->String que recebe uma string s e um
--número inteiro n e retorna uma nova string t com k caracteres ’>’ inseridos no início
--de s. O valor de k deve ser tal que o comprimento de t seja igual a n. Obs: se n é
--menor que o comprimento de s, a função retorna a própria string s.
--Exemplo:
--pushRight "abc" 5 = ">>abc"
pushRight :: String -> Int -> String
pushRight s n
    | n <= length s = s
    | otherwise = replicate (n - length s) '>' ++ s

--9-Dada uma lista de caracteres [Char], e um caractere a, retornar quantos caracteres
--da lista são iguais a a.
--Exemplo:
--conta "ABCAABCDDA" "B" = 2
conta :: [Char] -> Char -> Int
conta lista a = length [x | x <- lista, x == a]

--10- Seja o cadastro de pessoas dado pela função a seguir:--
--pessoa rg--
-- | rg == 1 = ("João Silva", 12, ’m’)--
-- | rg == 2 ("Jonas Souza", 51, ’m’)-
-- ...--
-- | rg == 321 = ("Jocileide Strauss", 21, ’f’)--
-- | otherwise = ("Não há ninguém mais", 9999, ’x’)--
--Construa funções que retornem os seguintes dados:--
--(a) O nome da pessoa de menor idade até um determinado registro.--
--(b) A idade média de todas as pessoas até um dado registro.--
--(c) O número de pessoas do sexo masculino.--
--(d) O número do registro da pessoa de maior idade--

pessoa :: Int -> (String, Int, Char)
pessoa rg 
    | rg == 1 = ("João Silva", 12, 'm')
    | rg == 2 = ("Jonas Souza", 51, 'm')
    | rg == 3 = ("Isadora Souza", 19, 'f')
    | rg == 4 = ("Isabela Souza", 27, 'f')
    | rg == 5 = ("Guilherme Souza", 18, 'm')
    | otherwise = ("Não há ninguem", 99999, 'x')

-- for(x=1; x<= nRegistros; x++){lista.Add(pessoas x)} --

criarlista :: Int -> [(String, Int, Char)]
criarlista nRegistros = [pessoa x | x <- [1..nRegistros]]

pessoas :: Int -> [(String, Int, Char)]
pessoas nRegistros = [(x,y,z) | (x,y,z) <- criarlista nRegistros, y /= 99999]

--(a) O nome da pessoa de menor idade até um determinado registro.--
menorIdade :: Int -> Int
menorIdade nRegistros = minimum ([y | (_,y,_) <- pessoas nRegistros])

nomeMenorIdade :: Int -> [String]
nomeMenorIdade nRegistros = [x | (x,y,_) <- pessoas nRegistros, y == menorIdade nRegistros]

--(b) A idade média de todas as pessoas até um dado registro.--
soma :: Int -> Int
soma nRegistros = sum([y | (_,y,_) <- pessoas nRegistros])

media :: Int -> Float
media nRegistros = fromIntegral (soma nRegistros) / fromIntegral (length (pessoas nRegistros))

--(c) O número de pessoas do sexo masculino.--
numeroMasculinos :: Int -> Int
numeroMasculinos nRegistros = length ([z | (_,_,z) <- pessoas nRegistros, z == 'm'])

--(d) O número do registro da pessoa de maior idade--
maiorIdade :: Int -> Int
maiorIdade nRegistros = maximum  ([y | (_,y,_) <- pessoas nRegistros])

-- Retorna as tuplas das pessoas com a maior idade (caso haja empate)
pessoasMaisVelhas :: Int -> [(String, Int, Char)]
pessoasMaisVelhas nRegistros = [(x, y, z) | (x, y, z) <- pessoas nRegistros, y == maiorIdade nRegistros]

-- Adiciona o número de RG a cada pessoa (1, 2, 3...)
listarPessoasComRg :: Int -> [(Int, String, Int, Char)]
listarPessoasComRg nRegistros = [(rg, x, y, z) | (rg, (x, y, z)) <- zip [1..] (pessoas nRegistros)]


-- Retorna apenas o RG da(s) pessoa(s) com maior idade
rgsDasPessoasMaisVelhas :: Int -> [Int]
rgsDasPessoasMaisVelhas nRegistros = [rg | (rg, x, y, z) <- listarPessoasComRg nRegistros, y == maiorIdade nRegistros]
