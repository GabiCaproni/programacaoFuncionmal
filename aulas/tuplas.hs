
--data = (dia , mes , ano)
testeInicial lista = [1| x <- lista]
quadrados = [x*x |x <-[1..5]]
pares lista = [x| x <- lista, mod x 2 == 0]
paresAoQuadrado lista = [x*x | x <- lista, mod x 2 == 0]
tuplasAoQuadrado listaTupla = [(x*x, y*y) | (x,y) <- listaTupla]
filtrar lista = [x | x <- lista, x>10]
jogadorCamisas = [(x,y) | x <- [10,11], y <- ["Romario", "Rivaldo"]]
conjuntos = [2*x | x <- [1..5]]
trianguloEqui lista1 lista2 lista3 = [(x,y,z) | x <- lista1, y <- lista2, z <- lista3, x==y, y==z]
trianguloRet lista1 lista2 lista3 = [(x,y,z) | x <- lista1, y <- lista2, z <- lista3, (x*x + y*y == z*z)]
trianguloRetSemRep lista1 lista2 lista3 = [(x,y,z) | x <- lista1, y <- lista2, z <- lista3, (x*x + y*y == z*z)]