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

-- O()
-- Crea una cola vacía.
emptyQ :: Queue a

-- O() 
-- Dada una cola indica si la cola está vacía.
isEmptyQ :: Queue a -> Bool
 
-- O()
-- Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue :: a -> Queue a -> Queue a

-- O() 
-- Dada una cola devuelve el primer elemento de la cola.
firstQ :: Queue a -> a


-- O()
-- Dada una cola la devuelve sin su primer elemento.
dequeue :: Queue a -> Queue a
