module MapDos (
    Map,
    emptyM,
    assocM,
    lookupM,
    deleteM,
    keys
)

where

import Maybe

data Map k v = M [k] [v]
{- INV.REP.: en (M ks vs)
    * la clave en la posición i de ks, está asociada al valor en la posición i de vs.
    * ks y vs tienen la misma longitud.
-}

-- O(1)
-- Propósito: devuelve un map vacíos
emptyM :: Map k v
emptyM = M [] []

-- O(1)
-- Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M ks vs) = 
    if elem k ks
     then M ks (sobreescribirValorEnPos (posicionDeClave k ks) v vs)
     else M (k:ks) (v:vs)
    
sobreescribirValorEnPos :: Int -> v -> [v] -> [v]
sobreescribirValorEnPos 0 v (v':vs) = (v:vs)
sobreescribirValorEnPos n v (v':vs) = v' : sobreescribirValorEnPos (n-1) v vs


-- O(n)
-- Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M ks vs) = buscarValorEnPos (posicionDeClave k ks) vs

-- O(n)
-- (Funcion auxiliar) Propósito: retorna un valor de una lista de valores vs, ubicado en la posición n dada.
buscarValorEnPos :: Int -> [v] -> Maybe v
buscarValorEnPos _ []     = Nothing
buscarValorEnPos 0 (v:vs) = Just v
buscarValorEnPos n (v:vs) = buscarValorEnPos (n-1) vs 
 
-- O(n)
-- (Funcion auxiliar) Propósito: retorna la posición en la que se encuentra una clave en una lista de claves vs.
--                               Si no se encuentra la clave, retorna el resultado de recorrer toda la lista.
--                    Precond: la clave debe encontrarse en el la lista de claves dada
posicionDeClave :: Eq k => k -> [k] -> Int
posicionDeClave k []      = 0 
posicionDeClave k (k':ks) =
    if k == k'
     then 0 
     else 1 + posicionDeClave k ks

-- O(n)
-- Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M ks vs) = M (borrarKEn k ks) (borrarValorEnPos (posicionDeClave k ks) vs)

-- O(n)
-- (Funcion auxiliar) Propósito: borra una clave en una lista de claves.
borrarKEn :: Eq k => k -> [k] -> [k]
borrarKEn k []      = []
borrarKEn k (k':ks) =
    if k == k'
     then ks
     else k' : borrarKEn k ks

-- O(n)     
-- (Funcion auxiliar) Propósito: borra un valor en una lista de valores vs, en la posición dada n.
--                    Precond: n >= 0 && n <= length vs 
borrarValorEnPos :: Int -> [v] -> [v]
borrarValorEnPos 0 (v:vs) = vs 
borrarValorEnPos n (v:vs) = v : borrarValorEnPos (n-1) vs

-- O(1)
-- Propósito: devuelve las claves del map.
keys :: Eq k => Map k v -> [k]
keys (M ks vs) = ks