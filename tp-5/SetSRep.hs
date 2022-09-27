module SetSRep (
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

data Set a = S [a] Int
  {- INV.REP.: En (S xs n)
      * n indica la cantidad de elementos de la lista xs.
      * xs no puede tener elementos repetidos.
  -}

-- O(1)
-- Crea un conjunto vacío.
emptyS :: Set a
emptyS = (S [] 0)

-- O(n) siendo n el costo operacional de elem. 
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
-- Precond: el elemento dado no se encuentra en el conjunto.
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

-- O(n^2) ya que realiza la operacion addS(O(n)) por cada elemento de la lista dada.
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs n) s2 = unionS' xs s2

-- O(n^2) ya que realiza la operacion addS(O(n)) por cada elemento de la lista dada.
-- (Funcion auxiliar) dada una lista y un conjunto, añade los elementos de la lista al conjunto dado.
unionS' :: Eq a => [a] -> Set a -> Set a
unionS' [] s     = s
unionS' (x:xs) s = addS x (unionS' xs s)

-- O(1)
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a]
setToList (S xs _) = xs
