-- Ejercicio 1
-- Indicar el costo de heapsort :: Ord a => [a] -> [a] (de la práctica anterior) suponiendo que
-- el usuario utiliza una priority queue con costos logarítmicos de inserción y borrado (o sea, usa una
-- Heap como tipo de representación).

-- O(log n) siendo n la cantidad de elementos en la lista.
-- Propósito: dada una lista la ordena de menor a mayor utilizando una Priority Queue como estructura auxiliar.
heapSort :: Ord a => [a] -> [a]
heapSort xs = pqALista (listaAPq xs)

-- O(log n) siendo n la cantidad de elementos de la lista
-- Propósito: dada una lista de elementos, retorna una Priority Queue con los elementos de la lista dada.
listaAPq :: Ord a => [a] -> PriorityQueue a
listaAPq []     = emptyPQ
listaAPq (x:xs) = insertPQ x (listaAPq xs)

-- O(log n) siendo n la cantidad de elemetos de la PriorityQueue.
-- Propósito: dada una Priority Queue retorna una lista con los elementos de la PQ, respetando el orden.
pqALista :: Ord a => PriorityQueue a -> [a] 
pqALista pq = if (isEmptyPQ pq)
                then []
                else findMinPQ pq : (pqALista (deleteMinPQ pq))

-- Ejercicio 2
-- Implementar las siguientes funciones suponiendo que reciben un árbol binario que cumple los
-- invariantes de BST y sin elementos repetidos (despreocuparse por el hecho de que el árbol puede
-- desbalancearse al insertar o borrar elementos). En todos los costos, N es la cantidad de elementos
-- del árbol. Justificar por qué la implementación satisface los costos dados.

-- Propósito: dado un BST dice si el elemento pertenece o no al árbol.
-- Costo: O(log N)
belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST x EmptyT          = False
belongsBST x (NodeT y ti td) = 
    if x == y
     then True
     else if x < y
           then belongsBST x ti
           else belongsBST x td 
-- en promedio es O(log N) porque recorre solo una rama del arbol sin necesidad de recorrer las otras,
-- pero si el arbol tiene solo una rama es O(N)

-- Propósito: dado un BST inserta un elemento en el árbol.
-- Costo: O(log N)
insertBST :: Ord a => a -> Tree a -> Tree a
insertBST x EmptyT          = NodeT x EmptyT EmptyT 
insertBST x (NodeT y ti td) =
    if x == y 
     then NodeT x ti td
     else if x < y
           then NodeT y (insertBST x ti) td
           else NodeT y ti (insertBST x td)
-- nuevamente en promedio es O(log N) porque recorre solo una rama del arbol (en peor caso) para insertar.

-- Propósito: dado un BST borra un elemento en el árbol.
-- Costo: O(log N)
deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST x EmptyT          = EmptyT
deleteBST x (NodeT y ti td) =
    if x == y 
     then rearmarBST ti td
     else if x < y
           then NodeT y (deleteBST x ti) td
           else NodeT y ti (deleteBST x td)

-- (Funcion auxiliar) Propósito: dados dos BSTs, retorna un par con el maximo elemento del arbol izquierdo y
--                               el nuevo arbol resultante de sacar el elemento.
--                    Precond: ambos arboles son BSTs.
-- Costo: O(log N)
rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
rearmarBST EmptyT td = td
rearmarBST ti td     = 
    let (max, ti') = splitMaxBST ti 
    in NodeT max td ti' 
-- es costo O(log N) porque utiliza splitMaxBST
    
-- Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
-- Precond: el arbol es BST y no esta vacio.
-- Costo: O(log N)
splitMinBST :: Ord a => Tree a -> (a, Tree a)
splitMinBST (NodeT x EmptyT td) = (x, td)
splitMinBST (NodeT x ti td)     = 
    let (min, ti') = splitMinBST ti 
    in (min, NodeT min ti' td) 
-- es costo O(log N) porque en el peor caso se recorre una rama, ya que se conoce donde esta
-- el minimo de un BST.

-- Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
-- Precond: el arbol es BST y no esta vacio.
-- Costo: O(log N)
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
splitMaxBST (NodeT x ti EmptyT) = (x, ti)
splitMaxBST (NodeT x ti td)     =
    let (max, td') = splitMaxBST td
    in (max, NodeT max ti td')
-- es costo O(log N) porque en el peor caso se recorre una rama, ya que se conoce donde esta
-- el maximo de un BST.

-- Propósito: indica si el árbol cumple con los invariantes de BST.
-- Costo: O(N^2)
esBST :: Tree a -> Bool
esBST EmptyT          = True 
esBST (NodeT x ti td) = esMayorQueNodo x ti &&
                        esMenorQueNodo x td && 
                        esBST ti             && 
                        esBST td
-- preguntar por resolución y costo

-- (Funcion auxiliar) Propósito: indica si el elemento dado es menor que el elemento del nodo del arbol dado.
-- Costo: O(1)
esMenorQueNodo :: Ord a => a -> Tree a -> Bool
esMenorQueNodo x EmptyT        = True
esMenorQueNodo x (NodeT y _ _) = x < y 

-- (Funcion auxiliar) Propósito: indica si el elemento dado es mayor que el elemento del nodo del arbol dado.
-- Costo: O(1)
esMayorQueNodo :: Ord a => a -> Tree a -> Bool
esMayorQueNodo x EmptyT        = True
esMayorQueNodo x (NodeT y _ _) = x > y 

-- Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al
-- elemento dado.
-- Costo: O(log N)
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorA x EmptyT                  = Nothing
elMaximoMenorA x (NodeT y EmptyT EmptyT) = if x > y then Just y else Nothing 
elMaximoMenorA x (NodeT y ti     td)     =
    if x < y
     then elMaximoMenorA x ti
     else elMaximoMenorA x td
-- es costo O(log N) porque solo busca en una rama, gracias a las invariantes de BST.

-- Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al
-- elemento dado.
-- Costo: O(log N)
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
elMinimoMayorA x EmptyT                  = Nothing
elMinimoMayorA x (NodeT y EmptyT EmptyT) = if x < y then Just y else Nothing 
elMinimoMayorA x (NodeT y ti     td)     =
    if x < y
     then elMinimoMayorA x td
     else elMinimoMayorA x ti
-- es costo O(log N) porque solo busca en una rama, gracias a las invariantes de BST.

-- Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
-- nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
-- Costo: O(N^2)
balanceado :: Tree a -> Bool
balanceado EmptyT          = True
balanceado (NodeT x ti td) = abs (heightT ti - heightT td) <= 1
-- preguntar por costo e implementación.

-- (Funcion de práctica 3) Dado un árbol devuelve su altura.
-- Costo: O(N)
heightT :: Tree a -> Int
heightT EmptyT          = 0 
heightT (NodeT _ t1 t2) = 1 + max (heightT t1) (heightT t2)

-- Ejercicio 3
{-
Dada la siguiente interfaz y costos para el tipo abstracto Map:

emptyM :: Map k v
Costo: O(1).

assocM :: Ord k => k -> v -> Map k v -> Map k v
Costo: O(log K).

lookupM :: Ord k => k -> Map k v -> Maybe v
Costo: O(log K).

deleteM :: Ord k => k -> Map k v -> Map k v
Costo: O(log K).

keys :: Map k v -> [k]
Costo: O(K).

recalcular el costo de las funciones como usuario de Map de la práctica anterior, siendo K es la
cantidad de claves del Map. Justificar las respuestas.
-}

-- O(K log K) porque primero obtiene todas las claves con keys O(K) y despues utiliza
--            valuesParaKeys (en este caso k == K, o sea todos los elementos del map)
-- Propósito: obtiene los valores asociados a cada clave del map.
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m = valuesParaKeys (keys m) m

-- O(k * log K) siendo k la cantidad de elementos de la lista dada y 
--              porque utiliza lookupM O(log K) por cada elemento de la lista.
-- (Funcion auxiliar) Propósito: dada una lista de keys y un map, retorna
--                               la lista de claves asociadas a las keys dadas.
valuesParaKeys :: Eq k => [k] -> Map k v -> [Maybe v]
valuesParaKeys []     m = []
valuesParaKeys (k:ks) m = lookupM k m : valuesParaKeys ks m

-- O(k * log K) siendo k la cantidad de elementos de la lista dada y porque 
--              utiliza lookupM O(log K) por cada uno de estos elementos.
-- Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] _     = True 
todasAsociadas (k:ks) m = isJust (lookupM k m) && todasAsociadas ks m 

-- O(k * log K) siendo k la cantidad de elementos de la lista dada y porque 
--              utiliza assocM O(log K) por cada uno de estos elementos.
-- Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

-- O(K log K) porque primero obtiene todas las claves con keys O(K) y despues utiliza
--            tuplasParaKeys (en este caso k == K, o sea todos los elementos del map) 
-- Propósito: convierte un map en una lista de pares clave valor.
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m = tuplasParaKeys (keys m) m

-- O(k * log K) siendo k la cantidad de elementos de la lista dada y porque 
--              utiliza lookupM O(log K) por cada uno de estos elementos.
-- (Funcion auxiliar) Propósito: dada una lista de claves y un map, retorna una lista de pares 
--                               compuestos por cada llave dada con su valor asociado.
-- Precond: todas las keys dadas deben tener un valor asociado en el map dado.
tuplasParaKeys :: Eq k => [k] -> Map k v -> [(k, v)]
tuplasParaKeys []     _ = []
tuplasParaKeys (k:ks) m = (k, fromJust (lookupM k m)) : tuplasParaKeys ks m

-- O(k * log K) siendo k la cantidad de elementos de la lista dada y porque 
--              utiliza lookupM/assocM O(log K) por cada uno de estos elementos.
-- Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq []       = emptyM
agruparEq (kv:kvs) = agregarClaveValores kv (agruparEq kvs)

-- O(log K) porque utiliza lookupM/assocM que son O(log K) 
-- (Funcion auxiliar) Propósito: dado un par clave-valor y un map clave-valores, si la clave se encuentra en el 
--                               map asocia la clave, con los valores previos pero agregandole la clave dada.
--                               Si la clave no se encuentra en el map, asocia la clave con una lista singular
--                               que contiene el valor dado.
agregarClaveValores :: Eq k =>  (k, v) -> Map k [v] -> Map k [v]
agregarClaveValores (k, v) m = 
    if isNothing (lookupM k m)
     then assocM k [v] m
     else assocM k (v : (fromJust (lookupM k m))) m

-- O(k * log K) siendo k la cantidad de elementos de la lista dada y porque 
--              utiliza lookupM/assocM O(log K) por cada uno de estos elementos. 
-- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
-- cada número asociado con dichas claves.
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar [] m     = m
incrementar (k:ks) m = 
    if isNothing (lookupM k m)
     then incrementar ks m
     else assocM k (1 + fromJust (lookupM k m)) (incrementar ks m)

-- O(K log K) ya que utiliza mapToList O(K log K) y despues agregarParesKV O(k log K),
--            en este caso k == K 
-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
-- una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = agregarParesKV (mapToList m1) m2

-- O(k * log K) siendo k la cantidad de elementos de la lista dada y porque 
--              utiliza lookupM/assocM O(log K) por cada uno de estos elementos. 
-- (Funcion auxiliar) Propósito: dada una lista de pares clave-valor y un map clave-valor,
--                               retorna el map con las claves dadas, asociadas a los valores dados.
--                               Si una clave ya existia en el map, se sobreescribe con el valor 
--                               dado.
agregarParesKV :: Eq k => [(k, v)] -> Map k v -> Map k v
agregarParesKV [] m          = m 
agregarParesKV ((k,v):kvs) m = assocM k v (agregarParesKV kvs m) 

-- O(k^2) ya que en el peor caso utiliza aumentar claves por cada elemento de la lista
-- Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
-- su posición en la lista.
indexar :: [a] -> Map Int a
indexar []     = emptyM
indexar (x:xs) = assocM 0 x (aumentarClaves (indexar xs))

-- O(K^2) ya que en el peor caso utiliza aumentar por cada elemento del map  
-- (Funcion auxiliar) Propósito: dado un Map Int a, aumenta en 1 todas las claves.
aumentarClaves :: Map Int a -> Map Int a
aumentarClaves m = listToMap (aumentar (mapToList m))

-- O(k) siendo k la cantidad de elementos en la lista dada
-- (Funcion auxiliar) Propósito: dada una lista de pares número-valor, aumenta en 1 el número de cada par.
aumentar :: [(Int, v)] -> [(Int, v)]
aumentar []          = []
aumentar ((n,v):nvs) = ((n+1), v) : aumentar nvs

-- O(s^2) ya que en el peor caso utiliza apariciones O(s^2) en el string dado
-- Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
-- en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias :: String -> Map Char Int
ocurrencias s = listToMap (apariciones s)

-- O(s^2) ya que utiliza cantApariciones por cada char del string
-- (Funcion auxiliar) Propósito: dado un string devuelve una lista de pares Char-Int que indican la cantidad
--                               de apariciones de un caracter en el string.
apariciones :: String -> [(Char, Int)]
apariciones []     = []
apariciones (c:cs) = (c, 1 + (cantApariciones c cs)) : apariciones cs

-- O(s) siendo k la cantidad de char en el string dado
-- (Funcion auxiliar) Propósito: dado un Char indica la cantidad de veces que aparece en un string dado.
cantApariciones :: Char -> String -> Int
cantApariciones c []     = 0
cantApariciones c (x:xs) = if c == x
						    then 1 + cantApariciones c xs
						    else cantApariciones c xs
