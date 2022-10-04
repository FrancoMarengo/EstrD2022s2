module MultiSet (
    MultiSet,
    emptyMS,
    addMS,
    ocurrencesMS,
    unionMS,
    intersectionMS,
    multiSetToList
)

where

-- import MapCRep
import MapSRep
-- import MapDos
import Maybe

data MultiSet a = MS (Map a Int)
-- sin invariantes de representacion

-- O(1)
-- Propósito: denota un multiconjunto vacío.
emptyMS :: MultiSet a
emptyMS = MS emptyM

-- O(N*m) siendo N la cantidad de elementos en el MultiSet y m el costo operacional de lookupM
-- Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
-- multiconjunto.
addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS x (MS m) = 
    let lkpx = lookupM x m in
    if isNothing lkpx 
     then MS (assocM x 1 m)
     else MS (assocM x (fromJust lkpx + 1) m)

-- O(N*m) siendo N la cantidad de elementos en el MultiSet y m el costo operacional de lookupM
-- Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
-- elemento en el multiconjunto.
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS x (MS m) = 
    let lkpx = lookupM x m in
    if isNothing lkpx 
     then 0
     else fromJust lkpx

-- O(n^2)
---- Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
---- ambos multiconjuntos.
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 
unionMS (MS m1) ms = agregarTuplasMS (mapToList m1) ms

-- O(n^2)
-- (Funcion auxiliar) Propósito: dada una lista de pares elemento-ocurrencias y un multiset, agrega 
--                               los elementos junto con sus ocurrencias al MS dado.
agregarTuplasMS :: Ord a => [(a,Int)] -> MultiSet a -> MultiSet a
agregarTuplasMS []          ms = ms 
agregarTuplasMS ((k,v):kvs) ms = addMSVeces k v (agregarTuplasMS kvs ms) 

-- O(n)
-- (Funcion auxiliar) Propósito: dado un elemento, un número y un MultiSet, agrega el elemento
--                               el número de veces dado al MS.
addMSVeces :: Ord a =>  a -> Int -> MultiSet a -> MultiSet a
addMSVeces k 0 ms = ms
addMSVeces k n ms = addMS k (addMSVeces k (n-1) ms)

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

-- Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
-- multiconjuntos tienen en común.
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 
intersectionMS (MS m1) (MS m2) = undefined

-- Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
-- su cantidad de ocurrencias.
multiSetToList :: Eq a => MultiSet a -> [(a, Int)]
multiSetToList (MS m) = mapToList m 