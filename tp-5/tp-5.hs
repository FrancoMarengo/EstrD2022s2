import SetSRep
-- import SetCRep
import Queue
-- import QueueInv

-- Calculo de costos

-- O(1)
head' :: [a] -> a
head' (x:xs) = x

-- O(1)
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

-- O(n) siendo n la distancia entre el número dado y el 0.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- O(n) siendo n la cantidad de elementos de la lista dada.
longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

-- O(n^2) siendo n la cantidad de elementos de la lista dada.
factoriales :: [Int] -> [Int]
factoriales []     = []
factoriales (x:xs) = factorial x : factoriales xs

-- O(n) siendo n la cantidad de elementos de la lista dada.
pertenece :: Eq a => a -> [a] -> Bool
pertenece n []     = False
pertenece n (x:xs) = n == x || pertenece n xs

-- O(n^2) siendo n la cantidad de elementos de la lista dada.
sinRepetidos' :: Eq a => [a] -> [a]
sinRepetidos' []     = []
sinRepetidos' (x:xs) =
    if pertenece x xs
     then sinRepetidos' xs
     else x : sinRepetidos' xs

-- O(n) siendo n la cantidad de elementos de la primer lista dada.
-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys     = ys
append (x:xs) ys = x : append xs ys

-- O(n^2) siendo n la cantidad de elementos de la lista dada.
concatenar :: [String] -> String
concatenar []     = []
concatenar (x:xs) = x ++ concatenar xs

-- O(n) siendo n el número dado.
takeN :: Int -> [a] -> [a]
takeN 0 xs     = []
takeN n []     = []
takeN n (x:xs) = x : takeN (n-1) xs

-- O(n) siendo n el número dado.
dropN :: Int -> [a] -> [a]
dropN 0 xs     = xs
dropN n []     = []
dropN n (x:xs) = dropN (n-1) xs

-- O(n) siendo n el número dado.
partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

-- O(n) siendo n la cantidad de elementos de la lista dada.
minimo :: Ord a => [a] -> a
minimo [x]    = x
minimo (x:xs) = min x (minimo xs)

-- O(n) siendo n la cantidad de elementos de la lista dada.
sacar :: Eq a => a -> [a] -> [a]
sacar n []     = []
sacar n (x:xs) =
    if n == x
     then xs
     else x : sacar n xs

-- O(n^2) siendo n la cantidad de elementos de la lista dada.
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs =
    let m  = minimo xs
    in m : ordenar (sacar m xs)

-- Set (usuario)

-- O(n*m) siendo n la cantidad de elementos de la lista dada y m el costo operacional de belongs.
-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
-- al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen []     _ = []
losQuePertenecen (x:xs) s =
    if (belongs x s)
     then x : losQuePertenecen xs s
     else losQuePertenecen xs s

-- O(n) siendo n el costo operacional de sinRepetidosS.
-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (sinRepetidosS xs)

-- O(n*m) siendo n la cantidad de elementos de la lista dada, y m el costo operacional de addS.
-- (Funcion auxiliar) crea un conjunto que no admite repetidos, con todos los elementos de la lista dada.
sinRepetidosS :: Eq a => [a] -> Set a
sinRepetidosS []     = emptyS
sinRepetidosS (x:xs) = addS x (sinRepetidosS xs)

-- O(n*m) siendo n la cantidad de nodos del arbol, y m el costo operacional de unionS.
-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
-- del arbol.
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT          = emptyS
unirTodos (NodeT s t1 t2) = unionS s (unionS (unirTodos t1) (unirTodos t2))

-- Tipo tree a definido para evitar error.
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

-- Queue

-- O(n) siendo n la cantidad de elementos de la cola.
-- Cuenta la cantidad de elementos de la cola.
lengthQ :: Queue a -> Int
lengthQ q =
    if (isEmptyQ q)
     then 0
     else 1 + (lengthQ (dequeue q))

-- O(n*m) siendo n la cantidad de elementos de la cola y m el costo operacional de la funcion isEmptyQ
-- Dada una cola devuelve la lista con los mismos elementos, donde el orden de la lista es el de la cola.
-- Nota: chequear que los elementos queden en el orden correcto.
queueToList :: Queue a -> [a]
queueToList q =
    if (isEmptyQ q)
     then []
     else firstQ q : queueToList (dequeue q)

-- O(n*m) siendo n la cantidad de elementos de q1 y m el costo operacional de la funcion isEmptyQ
-- Inserta todos los elementos de la segunda cola en la primera
unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = 
    if (isEmptyQ q1)
     then q2
     else enqueue (firstQ q1) (unionQ (dequeue q1) q2) 

-- Stack

-- O(n*m) siendo n la cantidad de elementos de la lista dada, y m el costo
--        operacional de la funcion push. 
-- Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar :: [a] -> Stack a
apilar []     = emptyS
apilar (x:xs) = push x (apilar xs)

-- O(n*m) siendo n la cantidad de elementos de la lista dada, y m el costo
--        operacional de la funcion isEmptyS. 
-- Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar :: Stack a -> [a]
desapilar s =
    if (isEmptyS s)
     then []
     else top s : desapilar (pop s)

-- O(n*m) siendo n la cantidad de elementos de la lista dada, y m el costo
--        operacional de la funcion push.
-- Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
-- posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos 0 x s = push x s 
insertarEnPos n x s = push (top s) (insertarEnPos (n-1) x (pop s))
