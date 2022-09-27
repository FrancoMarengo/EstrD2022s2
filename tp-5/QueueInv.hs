module QueueInv (
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

-- O(1) 
-- Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue :: a -> Queue a -> Queue a
enqueue x (Q xs) = Q (x:xs)

-- O(m) siendo m el costo operacional de la funcion ultimoElem.
-- Dada una cola devuelve el primer elemento de la cola.
-- Precond: debe haber al menos un elemento en la cola.
firstQ :: Queue a -> a
firstQ (Q xs) = ultimoElem xs

-- O(n) siendo n la cantidad de elementos de la lista dada.
-- (Funcion auxiliar) dada una lista retorna su ultimo elemento.
ultimoElem :: [a] -> a
ultimoElem (x:[]) = x 
ultimoElem (x:xs) = ultimoElem xs

-- O(m) siendo m el costo operacional de la funcion sinUltimoElem.
-- Dada una cola la devuelve sin su primer elemento.
-- Precond: la cola debe tener al menos un elemento
dequeue :: Queue a -> Queue a
dequeue (Q xs) = (Q (sinUltimoElem xs))

-- O(n) siendo n el tamaño de la lista dada.
-- (Funcion auxiliar) dada una lista, retorna la lista sin su ultimo elemento.
sinUltimoElem :: [a] -> [a]
sinUltimoElem (x:[]) = []
sinUltimoElem (x:xs) = x : sinUltimoElem xs
