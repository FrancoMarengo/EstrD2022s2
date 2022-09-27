module SetCRep (
    Set,
    emptyS,
    addS,
    belongs,
    sizeS,
    removeS,
    unionS,
    setToList
)

where

data Set a = S [a]
-- No posee invariantes de representación.

-- O(1)
-- Crea un conjunto vacío.
emptyS :: Set a
emptyS = (S [])

-- O(1) 
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS :: Eq a => a -> Set a -> Set a
addS x (S ys) = (S (x:ys))

-- O(n) siendo n el costo operacional de elem.
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs :: Eq a => a -> Set a -> Bool
belongs x (S ys) = elem x ys

-- O(n) siendo n el costo operacional de length.
-- Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int
sizeS (S xs) = length xs 

-- O(n) siendo n el costo operacional de removeS'
-- Borra un elemento del conjunto.
-- Precond: El elemento dado debe estar en el conjunto.
removeS :: Eq a => a -> Set a -> Set a
removeS x (S ys) = (S (removeS' x ys))

-- O(n) siendo n la cantidad de elementos de la lista dada.
-- (Funcion auxiliar) dado un elemento y una lista de elementos, elimina el elemento dado de la lista.
-- Precond: El elemento dado debe estar en el conjunto.
removeS' :: Eq a => a -> [a] -> [a]
removeS' x []     = error "El elemento no se encontraba en la lista dada"
removeS' x (y:ys) = 
    if (x == y)
     then ys
     else y : removeS' x ys

-- O(1)
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs) (S ys) = (S (xs++ys))

-- O(n) siendo n el costo operacional de sinRepetidosL
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a]
setToList (S xs) = sinRepetidosL xs

-- O(n^2) siendo n la cantidad de elementos de la lista.
-- (Funcion auxiliar) dada una lista retorna una lista sin repetidos.
sinRepetidosL :: Eq a => [a] -> [a]
sinRepetidosL []     = []
sinRepetidosL (x:xs) =
    if (elem x xs)
     then sinRepetidosL xs
     else x : (sinRepetidosL xs)
