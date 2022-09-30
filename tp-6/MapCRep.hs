module MapCRep (
    Map,
    emptyM,
    assocM,
    lookupM,
    deleteM,
    keys
)

where

data Map k v = M [(k, v)]
-- Sin invariantes de representación.

-- O(1)
-- Propósito: devuelve un map vacíos
emptyM :: Map k v
emptyM = M []

-- O(1)
-- Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M kvs) = M ((k,v):kvs)

-- O(n)
-- Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M kvs) = buscarValor k kvs

-- O(n) 
-- (Funcion auxiliar) Propósito: encuentra un valor dado una clave, en una lista de pares clave-valor.
buscarValor :: Eq k => k -> [(k, v)] -> Maybe v
buscarValor k []            = Nothing
buscarValor k ((k',v'):kvs) =
    if k == k'
     then Just v'
     else buscarValor k kvs

-- O(n)
-- Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M kvs) = M (borrarAsoc k kvs)

-- O(n)
-- (Funcion auxiliar) Propósito: borra una asociación dada una clave, en una lista de pares clave-valor.
borrarAsoc :: Eq k => k -> [(k, v)] -> [(k, v)]
borrarAsoc k []             = []
borrarAsoc k ((k', v'):kvs) =
    if k == k'
     then borrarAsoc k kvs
     else (k', v') : borrarAsoc k kvs

-- O(n^2)
-- Propósito: devuelve las claves del map.
keys :: Eq k => Map k v -> [k]
keys (M kvs) = sinClavesRepetidas (claves kvs)

-- O(n)
-- (Funcion auxiliar) Propósito: devuelve las claves de una lista de pares clave-valor.
claves :: [(k, v)] -> [k]
claves []           = []
claves ((k, v):kvs) = k : claves kvs 

-- O(n^2)
-- (Funcion auxiliar) Propósito: elimina las claves repetidas de una lista de claves.
sinClavesRepetidas :: Eq k => [k] -> [k]
sinClavesRepetidas []     = []
sinClavesRepetidas (k:ks) =
    if elem k ks
     then sinClavesRepetidas ks
     else k : sinClavesRepetidas ks