module SetCRep (
    SetCRep,
    emptyS,
    addS,
    belongs,
    sizeS,
    removeS,
    unionS,
    setToList
)

where

data SetSRep a = S [a]
-- No posee invariantes de representación.

-- O(1)
-- Crea un conjunto vacío.
emptyS :: Set a
emptyS = (S [] 0)

-- O(n) siendo n el costo operacional de elem. 
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS :: Eq a => a -> Set a -> Set a
addS x (S ys n) =
    if (elem x ys)
     then (S ys n)
     else (S (x:ys) (n+1))

-- O(n) siendo n el costo operacional de elem.
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs :: Eq a => a -> Set a -> Bool
belongs x (S ys _) = elem x ys

-- O(1)
-- Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int
sizeS (S _ n) = n

-- O(n*m) siendo n el costo operacional de removeS' y m el costo operacional de elem.
-- Borra un elemento del conjunto.
-- Precond: El elemento dado debe estar en el conjunto.
removeS :: Eq a => a -> Set a -> Set a
removeS x (S ys n) = 
    if (elem x ys)
     then (S (removeS' x ys) (n-1))
     else error "El elemento no estaba en el conjunto dado."

-- O(n) siendo n la cantidad de elementos de la lista dada.
-- (Funcion auxiliar) dado un elemento y una lista de elementos, elimina el elemento dado de la lista.
removeS' :: Eq a => a -> [a] -> [a]
removeS' x (y:ys) = 
    if (x == y)
     then ys
     else y : removeS' x ys

-- O(n) siendo n el costo operacional de la función '++' entre la lista xs e ys.
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs n) (S ys m) = (S (xs++ys) (n+m))

-- O(1)
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a]
setToList (S xs _) = xs
)