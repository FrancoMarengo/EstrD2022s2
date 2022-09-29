module PriorityQueue (
    PriorityQueue,
    emptyPQ,
    isEmptyPQ,
    insertPQ,
    findMinPQ,
    deleteMinPQ
)

where

data PriorityQueue a = PQ [a] 
{- INV.REP.: en (PQ xs)
    * la lista xs se encuentra ordenada de menor a mayor.
-}

-- O(1)
-- Propósito: devuelve una priority queue vacía.
emptyPQ :: PriorityQueue a
emptyPQ = PQ [] 

-- O(1)
-- Propósito: indica si la priority queue está vacía.
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ xs) = null xs

-- O(n)
-- Propósito: inserta un elemento en la priority queue.
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (PQ xs) = PQ (agregarPorOrden x xs)

-- O(n)
-- (Funcion auxiliar) Propósito: inserta un elemento a la lista teniendo en cuenta el orden.
--                    Precond: la lista se encuentra ordenada de menor a mayor.
agregarPorOrden :: Ord a => a -> [a] -> [a]
agregarPorOrden x []     = x : []
agregarPorOrden x (y:ys) = if x < y
                            then x : (y:ys)
                            else y : agregarPorOrden x ys

-- O(1)
-- Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
-- Precondición: parcial en caso de priority queue vacía.
findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (PQ xs) = head xs

-- O(1)
-- Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
-- Precondición: parcial en caso de priority queue vacía.
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (PQ xs) = PQ (tail xs)
