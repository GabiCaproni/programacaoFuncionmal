--Implemente uma função meuAbs que devolve o valor absoluto sem usar abs.--
--Exemplo:--
--meuAbs (-5) → 5--
--meuAbs 7 → 7--

meuAbs x = if x<0 
    then x*(-1)
    else x

--Crie uma função que receba dois números e devolva o maior deles.
--Exemplo:
--maior2 10 7 → 10
--maior2 (-3) 5 → 5
maior x y = if x>y 
    then x
    else y

--Escreva uma função que diga se um número é positivo, negativo ou zero.--
--Exemplo:--
--classificaSinal (-8) → "Negativo"--
--classificaSinal 0 → "Zero"--
--classificaSinal 42 → "Positivo"--

classificaSinal x = x>0
    then 'Positivo'
    else 'Negativo'