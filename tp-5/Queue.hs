module Queue (
    Queue,
    emptyQ,
    isEmptyQ,
    enqueue,
    firstQ,
    dequeue
)

where

data Queue a = Q [a]
-- Sin invariantes de representacion.

-- O(1)
-- Crea una cola vacía.
emptyQ :: Queue a
emptyQ = Q []

-- O(m) siendo m el costo operacional de isEmpty.
-- Dada una cola indica si la cola está vacía.
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs) = null xs 

-- O(m) siendo m el costo operacional de la funcion agregarAlFinal
-- Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue :: a -> Queue a -> Queue a
enqueue x (Q xs) = Q (agregarAlFinal xs x)

-- O(n) siendo n la cantidad de elementos de la lista dada
-- (Funcion auxiliar) Dados una lista y un elemento, devuelve una lista 
--                    con ese elemento agregado al final de la lista.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e     = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

-- O(m) siendo m el costo operacional de la funcion head.
-- Dada una cola devuelve el primer elemento de la cola.
firstQ :: Queue a -> a
firstQ (Q xs) = head xs

-- O(m) siendo m el costo operacional de la funcion tail.
-- Dada una cola la devuelve sin su primer elemento.
dequeue :: Queue a -> Queue a
dequeue (Q xs) = (Q (tail xs))
