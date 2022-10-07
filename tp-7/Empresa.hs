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

-- import MapBST
-- import SetBST

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
-- Costo: calcular.
agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa

-- Propósito: agrega un sector al empleado con dicho CUIL.
-- Costo: calcular.
agregarASector :: SectorId -> CUIL -> Empresa -> Empresa

-- Propósito: elimina al empleado que posee dicho CUIL.
-- Costo: calcular.
borrarEmpleado :: CUIL -> Empresa -> Empresa
