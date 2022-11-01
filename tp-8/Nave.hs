module Nave(
    Nave,
    construir
)

where

-- imports necesarios para funcionar
-- import MapBst
-- import MaxHeap
-- import Sector
-- import Tripulante

type SectorId = String
type Nombre = String
type Rango = String

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)

-- a)
{- INV.REP:
    * Un tripulante no puede repetirse en el MaxHeap.
    * Si un tripulante esta en un sector del Map SectorId-Sector, entonces debe estar en el
      Map Nombre-Tripulante y en el MaxHeap (no necesariamente viceversa).
    * El Map Nombre-Tripulante y el MaxHeap deben tener la misma cantidad de elementos (tripulantes).
   OBSERVACIONES:
    * Si un Sector no tiene tripulantes, entonces está vacio.
    * Si el MaxHeap y el Map Nombre-Tripulante están vacios, entonces la nave esta vacía.
-}

-- b)
-- Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
-- Eficiencia: O(S)
construir :: [SectorId] -> Nave
construir []       = N emptyM emptyM emptyH
construir (id:ids) = construirSectorEnNave id (construir ids)

-- (Funcion auxiliar) Propósito: Construye un sector en base a un identificador de sector y lo incorpora a la nave dada.
-- Eficiencia: O(log S) porque realiza pattern matching sobre la nave (O(1)), se realiza la operacion crearS (O(1))
--                      simplificandose al ser constantes, despues realiza las operaciones lookupM (O(log S))
--                      y assocM (O(log S)), simplificando la suma en O(log S).
construirSectorEnNave :: SectorId -> Nave -> Nave
construirSectorEnNave id (N ms mt mh) = 
    case lookupM id ms of
        Just x  -> N ms mt mh
        Nothing -> N (assocM id (crearS id) ms) mt mh

-- c)
-- Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
-- Eficiencia: O(log T)
ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT n r (N ms mt mh) = 
    case lookupM n mt of 
        Just x  -> N ms mt mh
        Nothing -> let t = crearT n r 
                    in N ms (assocM n t mt) (insertH t mh)

-- d)
-- Propósito: Devuelve los sectores asignados a un tripulante.
-- Precondición: Existe un tripulante con dicho nombre.
-- Eficiencia: O(log M) porque en peor caso se realiza lookupM recorriendo todo el map hasta encontrar al tripulante
--             y esto tiene costo O(log M), posteriormente se realiza sectoresT O(1) que se simplifica en el costo final.
sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados n (N _ mt _) = 
    case lookupM n mt of
        Just t  -> sectoresT t 
        Nothing -> error "No existe el tripulante en la nave"

-- e)
-- Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
-- Precondición: Existe un sector con dicho id.
-- Eficiencia: O(log S) porque en peor caso se realiza lookupM recorriendo todo el map hasta encontrar al sector
--             y esto tiene costo O(log S), posteriormente se realiza sectoresT O(1) y componentesS O(1) que se
--             simplifican en el costo final.
datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
datosDeSector id (N ms _ _) =
    case lookupM id ms of
        just s  -> (tripulantesS, componentesS)
        nothing -> error "No existe un sector con el id dado"
    
-- f)
-- Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
-- Eficiencia: O(log T)
tripulantesN :: Nave -> [Tripulante]
tripulantesN (N _ _ mh) = 