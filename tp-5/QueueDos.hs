module QueueDos (
    Queue,
    emptyQ,
    isEmptyQ,
    enqueue,
    firstQ,
    dequeue
)

where

data Queue a = Q [a] [a]
 {- INV.REP.: en (Q fs bs)
     * Si fs se encuentra vacía, entonces la cola se encuentra vacía.
 -}

-- O(1)
-- Crea una cola vacía.
emptyQ :: Queue a
emptyQ = Q [] []

-- O(1) 
-- Dada una cola indica si la cola está vacía.
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q fs bs) = null fs
 
-- O(1)
-- Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue :: a -> Queue a -> Queue a
enqueue x q =
    if isEmptyQ q
     then agregarElemFs x q 
     else agregarElemBs x q 

-- O(1)
-- (Funcion auxiliar) dado un elemento y una cola, agrega el elemento al front stack de la cola.
agregarElemFs :: a -> Queue a -> Queue a
agregarElemFs x (Q fs bs) = Q (x:fs) bs

-- O(1)
-- (Funcion auxiliar) dado un elemento y una cola, agrega el elemento al back stack de la cola.
agregarElemBs :: a -> Queue a -> Queue a 
agregarElemBs x (Q fs bs) = Q fs (x:bs)

-- O(1) 
-- Dada una cola devuelve el primer elemento de la cola.
firstQ :: Queue a -> a
firstQ (Q fs _) = head fs 

-- O(n) porque la funcion reverse es lineal y se utiliza en el caso que la fs este vacia (peor caso).
-- Dada una cola la devuelve sin su primer elemento.
dequeue :: Queue a -> Queue a
dequeue (Q fs bs) =
    if null (tail fs)
     then Q (reverse bs) []
     else Q (tail fs) bs
