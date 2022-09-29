import PriorityQueue
-- Priority queue

-- Propósito: dada una lista la ordena de menor a mayor utilizando una Priority Queue como estructura auxiliar.
heapSort :: Ord a => [a] -> [a]
heapSort xs = pqALista (listaAPq xs)

listaAPq :: Ord a => [a] -> PriorityQueue a
listaAPq []     = emptyPQ
listaAPq (x:xs) = insertPQ x (listaAPq xs)

pqALista :: Ord a => PriorityQueue a -> [a] 
pqALista pq = if (isEmptyPQ pq)
                then []
                else findMinPQ pq : (pqALista (deleteMinPQ pq)) 

