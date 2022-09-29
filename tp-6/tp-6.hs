import PriorityQueue

-- Priority queue

-- O(n)
-- Propósito: dada una lista la ordena de menor a mayor utilizando una Priority Queue como estructura auxiliar.
heapSort :: Ord a => [a] -> [a]
heapSort xs = pqALista (listaAPq xs)

-- O(n)
-- Propósito: dada una lista de elementos, retorna una Priority Queue con los elementos de la lista dada.
listaAPq :: Ord a => [a] -> PriorityQueue a
listaAPq []     = emptyPQ
listaAPq (x:xs) = insertPQ x (listaAPq xs)

-- O(n)
-- Propósito: dada una Priority Queue retorna una lista con los elementos de la PQ, respetando el orden.
pqALista :: Ord a => PriorityQueue a -> [a] 
pqALista pq = if (isEmptyPQ pq)
                then []
                else findMinPQ pq : (pqALista (deleteMinPQ pq)) 

-- Map (diccionario)

-- O(n^2) ya que utiliza valuesParaKeys.
-- Propósito: obtiene los valores asociados a cada clave del map.
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m = valuesParaKeys (keys m) m

-- O(n^2) ya que por cada elemento se realiza lookupM, y ésta en el peor de los casos 
--        recorre todo el map.
-- (Funcion auxiliar) Propósito: dada una lista de keys y un map, retorna
--                               la lista de claves asociadas a las keys dadas.
valuesParaKeys :: Eq k => [k] -> Map k v -> [Maybe v]
valuesParaKeys []     m = []
valuesParaKeys (k:ks) m = lookupM k m : valuesParaKeys ks m

-- O(n^2) ya que por cada elemento se realiza lookupM, y ésta en el peor de los casos 
--        recorre todo el map.
-- Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] _     = True 
todasAsociadas (k:ks) m = not (isNothing (lookupM k m)) && todasAsociadas ks m 

-- O(n*M) siendo n la cantidad de elementos de la lista dada y M el costo operacional
--        de assocM, ya que depende de su implementacion.
-- Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

-------------------------------------------
-------------------------------------------

-- Propósito: convierte un map en una lista de pares clave valor.
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m = tuplasParaKeys (keys m) m

tuplasParaKeys :: Eq k => [k] -> Map k v -> [(k, v)]
tuplasParaKeys []     _ = []
tuplasParaKeys (k:ks) m = (k, lookupM k m) : tuplasParaKeys ks m

-- Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq :: Eq k => [(k, v)] -> Map k [v]

-- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
-- cada número asociado con dichas claves.
incrementar :: Eq k => [k] -> Map k Int -> Map k Int

-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
-- una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
