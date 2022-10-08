module Empresa (
    Empresa,
    consEmpresa,
    buscarPorCUIL,
    empleadosDelSector,
    todosLosCUIL,
    todosLosSectores,
    agregarSector,
    agregarEmpleado,
    agregarASector,
    borrarEmpleado
)

where

-- imports necesarios para funcionar:

-- import MapBST
-- import SetBST
-- import Empleado

type SectorId = Int
type CUIL = Int

data Empresa = ConsE (Map SectorId (Set Empleado)) 
                     (Map CUIL Empleado)
{- INV.REP.: en (ConsE ms mc)
    * si un empleado aparece en mc, debe estar en al menos un sector de ms y viceversa
    * en mc no puede haber un empleado asignado a mas de un cuil
   OBSERVACIONES:
    * el primer map relaciona id de sectores con los empleados que trabajan en dicho sector.
    * el segundo map relaciona empleados con su número de CUIL.
    * un empleado puede estar asignado a más de un sector
-}

-- Propósito: construye una empresa vacía.
-- Costo: O(1)
consEmpresa :: Empresa
consEmpresa = ConsE emptyM emptyM

-- Propósito: devuelve el empleado con dicho CUIL.
-- Precond: el empleado con el cuil dado debe estar asignado a la empresa dada.
-- Costo: O(log E)
buscarPorCUIL :: CUIL -> Empresa -> Empleado
buscarPorCUIL c (ConsE _ mc) = fromJust(lookupM c mc)

-- Propósito: indica los empleados que trabajan en un sector dado.
-- Precond: el SectorId dado debe existir en la empresa dada.
-- Costo: O(logS + E)
empleadosDelSector :: SectorId -> Empresa -> [Empleado]
empleadosDelSector id (ConsE ms _) = setToList (fromJust (lookupM id ms))

-- Propósito: indica todos los CUIL de empleados de la empresa.
-- Costo: O(E)
todosLosCUIL :: Empresa -> [CUIL]
todosLosCUIL (ConsE _ mc) = keys mc

-- Propósito: indica todos los sectores de la empresa.
-- Costo: O(S)
todosLosSectores :: Empresa -> [SectorId]
todosLosSectores (ConsE ms _) = keys ms

-- Propósito: agrega un sector a la empresa, inicialmente sin empleados.
-- Costo: O(logS)
agregarSector :: SectorId -> Empresa -> Empresa
agregarSector id (ConsE ms mc) = ConsE (assocM id emptyS ms) mc 

-- Propósito: agrega un empleado a la empresa, en el que trabajará en dichos sectores y tendrá
-- el CUIL dado.
-- Precond: los sectorId dados deben estar en la empresa dada
-- Costo: O(s * M * log S * log E) siendo s la cantidad de sectores dados, M el costo operacional de
--                                 incorporarSector (logaritmico), S la cantidad de sectores
--                                 de la empresa y E la cantidad de empleados de la empresa.
agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarEmpleado ss c (ConsE ms mc) = 
    let empleado = incorporarSectores ss (consEmpleado c) 
    in ConsE (agregarASectores ss empleado ms) (registrar empleado mc)

-- (Funcion auxiliar) Propósito: registra un empleado dado en el map de cuil-Empleado
-- Costo: O(log C) siendo C la cantidad de cuil's en mc, debido a que usa assocM
registrar :: Empleado -> Map CUIL Empleado -> Map CUIL Empleado
registrar e mc = assocM (cuil e) e mc

-- (Funcion auxiliar) Propósito: dado una lista de sectores y un empleado, incluye los sectores
--                               a la estructura interna de empleado.
-- Costo: O(s * log S) siendo s la cantidad de sectores dados y S la cantidad de sectores que el 
--                     empleado tiene asignados.
incorporarSectores :: [SectorId] -> Empleado -> Empleado
incorporarSectores []     e = e 
incorporarSectores (s:ss) e = incorporarSector s (incorporarSectores ss e)

-- (Funcion auxiliar) Propósito: agrega un empleado a los sectores dados, en un map de sector-empleados
-- Costo: O(s * (log S * log E)) siendo s la cantidad de sectores dados y (log S * log E) el costo de
--                               la funcion incorporarASector 
agregarASectores :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
agregarASectores []     _ ms = ms 
agregarASectores (s:ss) e ms = incorporarASector s e (agregarASectores ss e ms)

-- (Funcion auxiliar) Propósito: agrega un empleado al sector dado, en un map de sector-empleados
--                    Precond: el sectorId dado se encuentra en el map dado
-- Costo: O(log S * log E) siendo S la cantidad de sectores en el map y E la cantidad de empleados en el set
incorporarASector :: SectorId -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
incorporarASector s e ms = assocM s (addS e (fromJust (lookupM s ms))) ms

-- Propósito: agrega un sector al empleado con dicho CUIL.
-- Precond: el sectorId dado se encuentra en la empresa dada
-- Costo: O(M * log S * log E) siendo M el costo operacional e incorporarSector (logaritmico)
--                             S la cantidad de sectores de la empresa y E la cantidad de empleados
--                             de la empresa
agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
agregarASector s c (ConsE ms mc) = 
    let empleado = incorporarSector s (fromJust (lookupM c mc)) 
    in ConsE (incorporarASector s empleado ms) (assocM c empleado mc)

-- Propósito: elimina al empleado que posee dicho CUIL.
-- Precond: el cuil del empleado debe estar registrado en la empresa
-- Costo: O(log C * s * (log E * log S)) siendo C la cantidad de cuils en mc, s la cantidad de sectores
--                                       asignados en el empleado, E la cantidad de empleados en el set
--                                       y S la cantidad de sectores en el map ms.
borrarEmpleado :: CUIL -> Empresa -> Empresa
borrarEmpleado c (ConsE ms mc) = 
    let empleado = fromJust (lookupM c mc)
    in ConsE (borrarDeSectores (sectores empleado) empleado ms) (borrarDeCuils (cuil empleado) mc)

-- (Funcion auxiliar) Propósito: borra a un empleado de un map cuil-empleado, segun un cuil dado
--                    Precond: el cuil dado se encuentra registrado en el map
-- Costo: O(log C) siendo C la cantidad de cuils registrados en el map
borrarDeCuils :: CUIL -> Map CUIL Empleado -> Map CUIL Empleado
borrarDeCuils c mc = deleteM c mc

-- (Funcion auxiliar) Propósito: borra a un empleado de una lista de sectores dados en un map de sector-empleados
--                    Precond: los sectores dados se encuentran en el map dado
-- Costo: O(s * (log E * log S)) siendo s la longitud de la lista de sectores dados, E la cantidad de
--                               empleados en el set y S la cantidad de sectores en el map
borrarDeSectores :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
borrarDeSectores []     _ ms = ms
borrarDeSectores (s:ss) e ms = borrarDeSector s e (borrarDeSectores ss e ms)

-- (Funcion auxiliar) Propósito: borra a un empleado de un sector dado en un map de sector-empleados
--                    Precond: el sector dado se encuentra en el map dado
-- Costo: O(log E * log S) siendo E la cantidad de empleados en el set y S la cantidad de sectores en el map
borrarDeSector :: SectorId -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
borrarDeSector s e ms = assocM s (removeS e (fromJust (lookupM s ms))) ms 