module MapSRep (
    Map,
    emptyM,
    assocM,
    lookupM,
    deleteM,
    keys
)

where

data Map k v = M [(k, v)]
{- INV.REP.: en (M kvs)
    * en kvs no puede haber tuplas con claves repetidas.
-}

-- O(1)
-- Propósito: devuelve un map vacíos
emptyM :: Map k v
emptyM = M []

-- O(n)
-- Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M kvs) = M (asociar k v kvs)

-- O(n)
-- (Funcion auxiliar) Propósito: agrega asociación clave-valor a una lista de pares clave-valor. Si la
--                               clave ya se encuentra en el map, sobreescribe el valor con el valor dado.
asociar :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
asociar k v []             = (k, v) : []
asociar k v ((k', v'):kvs) =
    if k == k'
     then (k, v) : kvs
     else (k', v') : asociar k v kvs

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
     then kvs
     else (k', v') : borrarAsoc k kvs

-- O(n)
-- Propósito: devuelve las claves del map.
keys :: Map k v -> [k]
keys (M kvs) = claves kvs

-- O(n)
-- (Funcion auxiliar) Propósito: devuelve las claves de una lista de pares clave-valor.
claves :: [(k, v)] -> [k]
claves []           = []
claves ((k, v):kvs) = k : claves kvs 
