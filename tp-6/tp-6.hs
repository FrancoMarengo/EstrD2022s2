import PriorityQueue
import MapCRep
-- import MapSRep
-- import MapDos
import Maybe
import MultiSet

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
todasAsociadas (k:ks) m = isJust (lookupM k m) && todasAsociadas ks m 

-- O(n*M) siendo n la cantidad de elementos de la lista dada y M el costo operacional
--        de assocM, ya que depende de su implementacion.
-- Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

-- O(n^2) ya que utiliza tuplasParaKeys.
-- Propósito: convierte un map en una lista de pares clave valor.
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m = tuplasParaKeys (keys m) m

-- O(n^2) ya que por cada key realiza la operación lookupM (O(n) en el peor caso).
-- (Funcion auxiliar) Propósito: dada una lista de claves y un map, retorna una lista de pares 
--                               compuestos por cada llave dada con su valor asociado.
-- Precond: todas las keys dadas deben tener un valor asociado en el map dado.
tuplasParaKeys :: Eq k => [k] -> Map k v -> [(k, v)]
tuplasParaKeys []     _ = []
tuplasParaKeys (k:ks) m = (k, fromJust (lookupM k m)) : tuplasParaKeys ks m

-- O(n^2) ya que por cada par de la lista dada, realiza agregarClaveValores, que en el peor caso es lineal.
-- Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq []       = emptyM
agruparEq (kv:kvs) = agregarClaveValores kv (agruparEq kvs)

-- O(n*M) siendo n la cantidad de elementos en el map y M el costo operacional de assocM/lookupM
--        que en el peor caso alguna de las dos es lineal.
-- (Funcion auxiliar) Propósito: dado un par clave-valor y un map clave-valores, si la clave se encuentra en el 
--                               map asocia la clave, con los valores previos pero agregandole la clave dada.
--                               Si la clave no se encuentra en el map, asocia la clave con una lista singular
--                               que contiene el valor dado.
agregarClaveValores :: Eq k =>  (k, v) -> Map k [v] -> Map k [v]
agregarClaveValores (k, v) m = 
    if isNothing (lookupM k m)
     then assocM k [v] m
     else assocM k (v : (fromJust (lookupM k m))) m

-- O(n^2) ya que por cada clave dada, realiza lookupM/assocM que en el peor caso alguna es lineal.
-- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
-- cada número asociado con dichas claves.
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar [] m     = m
incrementar (k:ks) m = 
    if isNothing (lookupM k m)
     then incrementar ks m
     else assocM k (1 + fromJust (lookupM k m)) (incrementar ks m)

-- O(N*M) siendo N el tamaño del primer map dado y M el costo operacional de agregarParesKV, que en 
--        el peor caso es cuadrática.
-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
-- una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = agregarParesKV (mapToList m1) m2

-- O(N*M) siendo N la longitud de la lista dada y M el costo operacional de assocM, que en el peor caso es lineal.
--        (en ese caso la funcion es n^2)
-- (Funcion auxiliar) Propósito: dada una lista de pares clave-valor y un map clave-valor,
--                               retorna el map con las claves dadas, asociadas a los valores dados.
--                               Si una clave ya existia en el map, se sobreescribe con el valor 
--                               dado.
agregarParesKV :: Eq k => [(k, v)] -> Map k v -> Map k v
agregarParesKV [] m          = m 
agregarParesKV ((k,v):kvs) m = assocM k v (agregarParesKV kvs m) 

-- O(n^2)
-- Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
-- su posición en la lista.
indexar :: [a] -> Map Int a
indexar []     = emptyM
indexar (x:xs) = assocM 0 x (aumentarClaves (indexar xs))

-- O(n^2) ya que usa mapToList
-- (Funcion auxiliar) Propósito: dado un Map Int a, aumenta en 1 todas las claves.
aumentarClaves :: Map Int a -> Map Int a
aumentarClaves m = listToMap (aumentar (mapToList m))

-- O(n)
-- (Funcion auxiliar) Propósito: dada una lista de pares número-valor, aumenta en 1 el número de cada par.
aumentar :: [(Int, v)] -> [(Int, v)]
aumentar []          = []
aumentar ((n,v):nvs) = ((n+1), v) : aumentar nvs

-- O(n^2)
-- Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
-- en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias :: String -> Map Char Int
ocurrencias s = listToMap (apariciones s)

-- O(n^2)
-- (Funcion auxiliar) Propósito: dado un string devuelve una lista de pares Char-Int que indican la cantidad
--                               de apariciones de un caracter en el string.
apariciones :: String -> [(Char, Int)]
apariciones []     = []
apariciones (c:cs) = (c, 1 + (cantApariciones c cs)) : apariciones cs

-- O(n)
-- (Funcion auxiliar) Propósito: dado un Char indica la cantidad de veces que aparece en un string dado.
cantApariciones :: Char -> String -> Int
cantApariciones c []     = 0
cantApariciones c (x:xs) = if c == x
						    then 1 + cantApariciones c xs
						    else cantApariciones c xs

-- MultiSet

-- O(n*M) siendo n la cantidad de caracteres en el string y M el costo operacional de addMS.
-- Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
-- en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrenciasMS :: String -> MultiSet Char
ocurrenciasMS []     = emptyMS
ocurrenciasMS (c:cs) = addMS c (ocurrenciasMS cs) 