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