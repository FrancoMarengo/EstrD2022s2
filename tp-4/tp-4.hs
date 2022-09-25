-- Pizzas

data Pizza = Prepizza
           | Capa Ingrediente Pizza
           deriving Show 
data Ingrediente = Salsa
                 | Queso
                 | Jamon
                 | Aceitunas Int
                 deriving Show

piz = (Capa Salsa
          (Capa Jamon
              (Capa Queso
                  (Capa Queso
                  (Capa (Aceitunas 8) (Prepizza))))))

-- Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

-- Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza []     = Prepizza
armarPizza (i:is) = (Capa i (armarPizza is))

-- Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza   = Prepizza
sacarJamon (Capa i p) = if (esJamon i)
                         then sacarJamon p
                         else (Capa i (sacarJamon p))

-- (Funcion auxiliar) Indica si un ingrediente es Jamon.
esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False 

-- Dice si una pizza tiene salsa y queso
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza   = True
tieneSoloSalsaYQueso (Capa i p) = esSalsaOQueso i && tieneSoloSalsaYQueso p

-- (Funcion auxiliar) Indica si un ingrediente es Salsa o Queso
esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True 
esSalsaOQueso _     = False

-- Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = (Capa (duplicarAceitunas' i) (duplicarAceitunas p))

-- (Funcion auxiliar) Duplica la cantidad de aceitunas si el ingrediente es aceitunas, en cualquier otro caso retorna el ingrediente
duplicarAceitunas' :: Ingrediente -> Ingrediente
duplicarAceitunas' (Aceitunas n) = (Aceitunas (n*2))
duplicarAceitunas' i             = i 

-- Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
-- ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p) : cantCapasPorPizza ps

-- Mapa de tesoros (con bifurcaciones)

data Dir = Izq | Der
         deriving Show
data Objeto = Tesoro | Chatarra
            deriving Show
data Cofre = Cofre [Objeto]
           deriving Show
data Mapa = Fin Cofre
          | Bifurcacion Cofre Mapa Mapa
          deriving Show

mapa = (Bifurcacion (Cofre [])
           (Bifurcacion (Cofre [Chatarra, Chatarra])
               (Fin (Cofre []))
               (Fin (Cofre []))
            )
            (Bifurcacion (Cofre [Chatarra])
                (Bifurcacion (Cofre [])
                    (Fin (Cofre []))
                    (Fin (Cofre [Tesoro]))
                )
                (Fin (Cofre [Chatarra, Chatarra]))
            )
       )

-- 1
-- Indica si hay un tesoro en alguna parte del mapa.
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c)               = hayTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnCofre c || hayTesoro m1 || hayTesoro m2

-- (Funcion auxiliar) Dado un cofre, indica si hay un tesoro en él.
hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre os) = hayTesoroEnCofre' os

-- (Funcion auxiliar) Dada una lista de objetos, indica si hay un tesoro en ésta.
hayTesoroEnCofre' :: [Objeto] -> Bool
hayTesoroEnCofre' []     = False 
hayTesoroEnCofre' (o:os) = esTesoro o || hayTesoroEnCofre' os

-- (Funcion auxiliar) Indica si un objeto es Tesoro.
esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

-- 2
-- Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
-- lista vacía de direcciones.
-- PRECOND: La lista de direcciones debe llevar a un camino del mapa existente.
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn []     m = hayTesoroEnTramo m
hayTesoroEn (d:ds) m = hayTesoroEn ds (tomarDireccion d m)

-- (Funcion auxiliar) Dada una Dir y un Mapa, retorna el mapa resultante tras tomar la direccion indicada.
-- PRECOND: El Mapa dado no puede ser el final del camino.
tomarDireccion :: Dir -> Mapa -> Mapa 
tomarDireccion _ (Fin _)                = error "Llegaste al final del camino."
tomarDireccion Izq (Bifurcacion _ m1 _) = m1
tomarDireccion Der (Bifurcacion _ _ m2) = m2 

-- (Funcion auxiliar) Dado un Mapa, indica si hay un tesoro en ese tramo específico.
hayTesoroEnTramo :: Mapa -> Bool
hayTesoroEnTramo (Fin c)             = hayTesoroEnCofre c
hayTesoroEnTramo (Bifurcacion c _ _) = hayTesoroEnCofre c

-- 3
-- Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin _)               = []
caminoAlTesoro (Bifurcacion c m1 m2) = if (hayTesoroEnCofre c)
                                        then []
                                        else if (hayTesoro m1)
                                         then Izq : (caminoAlTesoro m1)
                                         else Der : (caminoAlTesoro m2)

-- 4
-- Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _)               = []
caminoDeLaRamaMasLarga (Bifurcacion _ m1 m2) = if (heightM m1 > heightM m2)
                                                then Izq : caminoDeLaRamaMasLarga m1
                                                else Der : caminoDeLaRamaMasLarga m2 

heightM :: Mapa -> Int
heightM (Fin _)               = 0
heightM (Bifurcacion _ m1 m2) = 1 + max (heightM m1) (heightM m2)

-- 5
-- Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c)               = tesorosDeCofre c : []
tesorosPorNivel (Bifurcacion c m1 m2) = (tesorosDeCofre c) : zipTesoros (tesorosPorNivel m1) (tesorosPorNivel m2)

tesorosDeCofre :: Cofre -> [Objeto]
tesorosDeCofre (Cofre os) = tesorosDeCofre' os

tesorosDeCofre' :: [Objeto] -> [Objeto]
tesorosDeCofre' []     = []
tesorosDeCofre' (o:os) = if (esTesoro o)
                          then o : tesorosDeCofre' os
                          else tesorosDeCofre' os 

zipTesoros :: [[Objeto]] -> [[Objeto]] -> [[Objeto]]
zipTesoros []  yss           = yss
zipTesoros xss []            = xss
zipTesoros (xs:xss) (ys:yss) = (xs++ys) : zipTesoros xss yss

-- 6
-- Devuelve todos lo caminos en el mapa.
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _)               = []
todosLosCaminos (Bifurcacion _ m1 m2) = consACada Izq (todosLosCaminos m1) ++ consACada Der (todosLosCaminos m2)

consACada :: a -> [[a]] -> [[a]]
consACada x []       = [[x]]
consACada x (ys:yss) = (x : ys) : (consACada x yss)  

-- Nave espacial
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
                deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible
            deriving Show 
data Sector = S SectorId [Componente] [Tripulante]
            deriving Show
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
            deriving Show 
data Nave = N (Tree Sector)
          deriving Show

nave = N (NodeT (S "s1" [LanzaTorpedos, (Almacen [Torpedo])] ["Juan", "Pepe"])
             (EmptyT)
             (NodeT (S "s2" [Motor 12] ["Luis"])
                 (NodeT (S "s2.2" [(Almacen [Comida, Oxigeno]), LanzaTorpedos] ["Franco"])
                     (EmptyT)
                     (NodeT (S "s3" [LanzaTorpedos] ["xd"])
                         (EmptyT)
                         (EmptyT)
                     )
                 )
                 (NodeT (S "s3.3" [Motor 100] ["Fantasma", "xd"])
                     (EmptyT)
                     (EmptyT) 
                 )
             )
         )

-- 1
-- Propósito: Devuelve todos los sectores de la nave.
sectores :: Nave -> [SectorId]
sectores (N ts) = sectores' ts

-- (Funcion auxiliar) Propósito: Devuelve todos los sectores de un árbol de sectores
sectores' :: Tree Sector -> [SectorId]
sectores' EmptyT          = []
sectores' (NodeT s t1 t2) = idSector s : sectores' t1 ++ sectores' t2

-- (Funcion auxiliar) Propósito: Devuelve el id de un sector.
idSector :: Sector -> SectorId
idSector (S id _ _) = id

-- 2
-- Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
-- el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int
poderDePropulsion (N ts) = poderDePropulsion' ts

-- (Funcion auxiliar) Propósito: Devuelve la suma de poder de propulsión de todos los motores de un árbol de sectores.s
poderDePropulsion' :: Tree Sector -> Int
poderDePropulsion' EmptyT          = 0
poderDePropulsion' (NodeT s t1 t2) = poderPropulsionSector s + (poderDePropulsion' t1) + (poderDePropulsion' t2)

-- (Funcion auxiliar) Propósito: Devuelve la suma de poder de propulsión de todos los motores de un sector.
poderPropulsionSector :: Sector -> Int
poderPropulsionSector (S _ cs _) = poderPropulsionComp cs

-- (Funcion auxiliar) Propósito: Devuelve la suma de poder de propulsión de todos los motores de una lista de componentes.
poderPropulsionComp :: [Componente] -> Int
poderPropulsionComp []     = 0
poderPropulsionComp (c:cs) = poderDePropulsionComp' c + poderPropulsionComp cs 

-- (Funcion auxiliar) Propósito: Devuelve la suma  de poder de propulsión de un componente.
poderDePropulsionComp' :: Componente -> Int
poderDePropulsionComp' (Motor p) = p
poderDePropulsionComp' _         = 0

-- 3
-- Propósito: Devuelve todos los barriles de la nave.
barriles :: Nave -> [Barril]
barriles (N ts) = barriles' ts

-- (Funcion auxiliar) Propósito: Devuelve todos los barriles de un árbol de sectores.
barriles' :: Tree Sector -> [Barril]
barriles' EmptyT          = []
barriles' (NodeT s t1 t2) = barrilesDeSector s ++ barriles' t1 ++ barriles' t2

-- (Funcion auxiliar) Propósito: Devuelve todos los barriles de un sector.
barrilesDeSector :: Sector -> [Barril]
barrilesDeSector (S _ cs _) = barrilesDeComp cs

-- (Funcion auxiliar) Propósito: Devuelve todos los barriles de una lista de componentes.
barrilesDeComp :: [Componente] -> [Barril]
barrilesDeComp []     = []
barrilesDeComp (c:cs) = barrilesDeComp' c ++ barrilesDeComp cs

-- (Funcion auxiliar) Propósito: Devuelve todos los barriles de un componente, si el componente no es un Almacen, entonces
-- retorna una lista vacía.
barrilesDeComp' :: Componente -> [Barril]
barrilesDeComp' (Almacen bs) = bs
barrilesDeComp' _            = []

-- 4
-- Propósito: Añade una lista de componentes a un sector de la nave.
-- Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs id (N ts) = (N (agregarASector' cs id ts))

-- (Funcion auxiliar) Propósito: Añade una lista de componentes a un sector de un árbol de sectores.
agregarASector' :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASector' _  _  EmptyT          = EmptyT
agregarASector' cs id (NodeT s t1 t2) = if (sonMismoSector id s)
                                         then (NodeT (agregarComponentes cs s) t1 t2)
                                         else (NodeT s (agregarASector' cs id t1) (agregarASector' cs id t2))

-- (Funcion auxiliar) Propósito: Indica si un id dado pertenece a un sector dado.
sonMismoSector :: SectorId -> Sector -> Bool
sonMismoSector id (S sid _ _) = id == sid

-- (Funcion auxiliar) Propósito: Añade una lista de componentes a un sector dado.
agregarComponentes :: [Componente] -> Sector -> Sector
agregarComponentes cs (S id cs' ts) = (S id (cs ++ cs') ts)

-- 5
-- Propósito: Incorpora un tripulante a una lista de sectores de la nave.
-- Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA tr ids (N ts) = N (asignarTripulanteA' tr ids ts)

-- (Funcion auxiliar) Propósito: Incorpora un tripulante a una lista de sectores del árbol de sectores.
asignarTripulanteA' :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteA' _  _   EmptyT          = EmptyT
asignarTripulanteA' tr ids (NodeT s t1 t2) = if (esSectorAAsignar s ids)
                                              then (NodeT (asignarTripulanteSector tr s) 
                                                              (asignarTripulanteA' tr ids t1)
                                                              (asignarTripulanteA' tr ids t2))
                                              else (NodeT s (asignarTripulanteA' tr ids t1)
                                                            (asignarTripulanteA' tr ids t2))


-- (Funcion auxiliar) Propósito: Indica si el id de un sector corresponde a una lista de id's de sector.
esSectorAAsignar :: Sector -> [SectorId] -> Bool
esSectorAAsignar (S sid _ _) ids = esSectorAAsignar' sid ids

-- (Funcion auxiliar) Propósito: Indica si un id de sector corresponde a una lista de id's de sector.
esSectorAAsignar' :: SectorId -> [SectorId] -> Bool
esSectorAAsignar' _   []       = False
esSectorAAsignar' sid (id:ids) = sid == id || esSectorAAsignar' sid ids

-- (Funcion auxiliar) Propósito: Incorpora un tripulante a un sector.
asignarTripulanteSector :: Tripulante -> Sector -> Sector
asignarTripulanteSector t (S id cs ts) = (S id cs (t:ts))

-- 6
-- Propósito: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N ts) = sectoresAsignados' t ts

-- (Funcion auxiliar) Propósito: Devuelve los sectores donde aparece un tripulante dado en un árbol de sectores.
sectoresAsignados' :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignados' _ EmptyT          = []
sectoresAsignados' t (NodeT s t1 t2) = if (estaAsignado t s)
                                        then idSector s : sectoresAsignados' t t1 ++ sectoresAsignados' t t2
                                        else sectoresAsignados' t t1 ++ sectoresAsignados' t t2

-- (Funcion auxiliar) Propósito: Indica si un tripulante esta asignado a un sector.
estaAsignado :: Tripulante -> Sector -> Bool
estaAsignado t (S _ _ ts) = estaAsignado' t ts 

-- (Funcion auxiliar) Propósito: Indica si un tripulante se encuentra en una lista de tripulantes.
estaAsignado' :: Tripulante -> [Tripulante] -> Bool
estaAsignado' _ []      = False 
estaAsignado' tr (t:ts) = tr == t || estaAsignado' tr ts

-- 7
-- Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes :: Nave -> [Tripulante]
tripulantes (N ts) = tripulantes' ts

-- (Funcion auxiliar) Propósito: Devuelve la lista de tripulantes de un árbol de sectores, sin tripulantes repetidos
tripulantes' :: Tree Sector -> [Tripulante]
tripulantes' EmptyT          = []
tripulantes' (NodeT s t1 t2) = sinRepetidos (tripulantesDeSector s ++ tripulantes' t1 ++ tripulantes' t2)

-- (Funcion auxiliar) Propósito: Retorna los tripulantes de un sector
tripulantesDeSector :: Sector -> [Tripulante] 
tripulantesDeSector (S _ _ ts) = ts

-- (Funcion auxiliar) Propósito: Dada una lista, retorna la misma lista sin repetidos.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos []     = []
sinRepetidos (x:xs) = if (estaEnLaLista x xs)
                       then sinRepetidos xs
                       else x : sinRepetidos xs

-- (Funcion auxiliar) Propósito: Dado un elemento y una lista de elementos, indica si el elemento se encuentra en la lista.
estaEnLaLista :: Eq a => a -> [a] -> Bool
estaEnLaLista _ []     = False 
estaEnLaLista x (y:ys) = x == y || estaEnLaLista x ys 

-- Manada de lobos
type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
          | Explorador Nombre [Territorio] Lobo Lobo
          | Cria Nombre
          deriving Show
data Manada = M Lobo
            deriving Show

-- 1 
-- Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean crías.
manada = M (Explorador "Asan" ["Selva", "Bosque", "Oscuro"]
               (Cazador "Predator" ["Pumba", "Timon"]
                   (Cria "Firulais")
                   (Cria "Queue")
                   (Explorador "Fiumba" ["Oscuro", "Pantano", "Bosque"]
                       (Cria "Jara")
                       (Cria "Monada")
                   )
                )
                (Cria "Stack"))

-- 2
-- Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
buenaCaza :: Manada -> Bool
buenaCaza m = alimentoCazado m > cantidadDeCrias m 

-- (Funcion auxiliar) Propósito: dada una manada, retorna la cantidad de alimento cazado.
alimentoCazado :: Manada -> Int
alimentoCazado (M l) = alimentoCazado' l

-- (Funcion auxiliar) Propósito: dado un lobo, retorna la cantidad de alimento cazado por el lobo o sus subordinados.
alimentoCazado' :: Lobo -> Int
alimentoCazado' (Cria _)                = 0 
alimentoCazado' (Explorador _ _ l1 l2)  = alimentoCazado' l1
                                        + alimentoCazado' l2 
alimentoCazado' (Cazador _ ps l1 l2 l3) = alimentoEn ps 
                                        + alimentoCazado' l1 
                                        + alimentoCazado' l2 
                                        + alimentoCazado' l3

-- (Funcion auxiliar) Propósito: dada una manada, retorna la cantidad de crias.
cantidadDeCrias :: Manada -> Int
cantidadDeCrias (M l) = cantidadDeCrias' l

-- (Funcion auxiliar) Propósito: dado un lobo, retorna la cantidad de crias que tiene el lobo o sus subordinados.
cantidadDeCrias' :: Lobo -> Int
cantidadDeCrias' (Cria _)               = 1 
cantidadDeCrias' (Explorador _ _ l1 l2) = cantidadDeCrias' l1
                                        + cantidadDeCrias' l2 
cantidadDeCrias' (Cazador _ _ l1 l2 l3) = cantidadDeCrias' l1
                                        + cantidadDeCrias' l2
                                        + cantidadDeCrias' l3 

-- (Funcion auxiliar) Propósito: retorna la cantidad de alimento en una lista de presas.
alimentoEn :: [Presa] -> Int
alimentoEn ps = length ps 

-- 3 
-- Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
--            con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
--            cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de cero presas.
elAlfa :: Manada -> (Nombre, Int)
elAlfa (M l) = elAlfa' l

-- (Funcion auxiliar) Propósito: dado un lobo y sus subordinados, devuelve el nombre del lobo con mas presas cazadas.
elAlfa' :: Lobo -> (Nombre, Int)
elAlfa' (Cria n)                = (n,0)
elAlfa' (Explorador n _ l1 l2)  = elegirEntre (elAlfa' l1) 
                                              (elegirEntre (elAlfa' l2) 
                                                           (n,0))
elAlfa' (Cazador n ps l1 l2 l3) = elegirEntre (elegirEntre (n, alimentoEn ps) 
                                                           (elAlfa' l1))
                                              (elegirEntre (elAlfa' l2) 
                                                           (elAlfa' l3))

-- (Funcion auxiliar) Propósito: dada dos tuplas que representan dos lobos con su cantidad de presas cazadas
--                               elige entre el lobo con mas presas cazadas, o el primer lobo, en caso de empate.
elegirEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elegirEntre (n1, c1) (n2, c2) = if (c1 >= c2)
                                 then (n1, c1)
                                 else (n2, c2)

-- 4
-- Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
--            pasaron por dicho territorio.
losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M l) = losQueExploraronL t l

-- (Funcion auxiliar) Propósito: dado un territorio y un lobo, devuelve los nombres de los exploradores
--                               que pasaron por dicho territorio.
losQueExploraronL :: Territorio -> Lobo -> [Nombre]
losQueExploraronL t (Cria _)                = []
losQueExploraronL t (Cazador _ _ l1 l2 l3)  = losQueExploraronL t l1
                                           ++ losQueExploraronL t l2
                                           ++ losQueExploraronL t l3 
losQueExploraronL t (Explorador n ts l1 l2) = if (esTerritorioExplorado t ts)
                                               then n : losQueExploraronL t l1 ++ losQueExploraronL t l2
                                               else losQueExploraronL t l1 ++ losQueExploraronL t l2 

-- (Funcion auxiliar) Propósito: dado un territorio, indica si se encuentra dentro de una lista de territorios
esTerritorioExplorado :: Territorio -> [Territorio] -> Bool
esTerritorioExplorado _  []     = False
esTerritorioExplorado te (t:ts) = te == t || esTerritorioExplorado te ts

-- 5
-- Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio
--            y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
--            dicho territorio. Los territorios no deben repetirse.
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M l) = exploradoresPorTerritorioL l

-- (Funcion auxiliar) Propósito: dado un lobo, denota la lista de los pares cuyo primer elemento es un territorio
--                               y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
--                               dicho territorio.
exploradoresPorTerritorioL :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioL (Cria _)                = []
exploradoresPorTerritorioL (Cazador _ _ l1 l2 l3)  = agregarExploradores (agregarExploradores (exploradoresPorTerritorioL l1)
                                                                                              (exploradoresPorTerritorioL l2))
                                                                         (exploradoresPorTerritorioL l3)
exploradoresPorTerritorioL (Explorador n ts l1 l2) = agregarExploradores (agregarExploradores (exploradoresPorTerritorioL l1)
                                                                                              (exploradoresPorTerritorioL l2))
                                                                         (territoriosDeLobo n ts)

-- (Funcion auxiliar) Propósito: dado un nombre de lobo, y una lista de territorios explorados por el lobo,
--                               denota la lista de los pares cuyo primer elemento es un territorio y el segundo
--                               elemento es una lista singular con el nombre del lobo.
territoriosDeLobo :: Nombre -> [Territorio] -> [(Territorio, [Nombre])]
territoriosDeLobo _ []     = []
territoriosDeLobo n (t:ts) = (t, [n]) : territoriosDeLobo n ts 

-- (Funcion auxiliar) Propósito: dada dos listas de tuplas de territorios y nombres de lobo que exploraron dichos territorios,
--                               denota una lista de tuplas de territorios y nombres, eliminando territorios repetidos y
--                               juntando los nombres de los lobos que exploraron mismos territorios.
agregarExploradores :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] 
agregarExploradores []       ts2 = ts2 
agregarExploradores (t1:ts1) ts2 = agregarExploradores ts1 (agregarExploradores' t1 ts2)

-- (Funcion auxiliar) Propósito: dada una tupla de territorio y lista singular de nombre de lobo, y una lista de tuplas
--                               donde el primer elemento es un territorio y el segundo elemento es una lista de nombres
--                               de lobo que exploraron dicho territorio, unifica el primer nombre de lobo dado con los 
--                               nombres de otros lobos que exploraron los mismo territorios que éste, eliminando
--                               tuplas con territorios repetidos.
agregarExploradores' :: (Territorio, [Nombre]) -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarExploradores' t1 []       = t1 : []
agregarExploradores' t1 (t2:ts2) = if (fst t1 == fst t2)
                                    then agregarExploradores' (fst t1, (snd t1 ++ snd t2)) ts2
                                    else t2 : agregarExploradores' t1 ts2

-- Propósito: dado un nombre de cazador y una manada, indica el nombre de todos los cazadores que tienen
-- como subordinado al cazador dado (directa o indirectamente).
-- Precondición: hay un cazador con dicho nombre y es único.
superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador n (M l) = superioresDelCazadorL n l

-- (Funcion auxiliar) Propósito: dado un nombre de cazador y un lobo, indica el nombre de todos los cazadores que tienen
--                               como subordinado al cazador dado (directa o indirectamente).
superioresDelCazadorL :: Nombre -> Lobo -> [Nombre]
superioresDelCazadorL nc (Cria _)                 = []
superioresDelCazadorL nc (Explorador _ _ l1 l2)   = if existeElCazador nc l1 
                                                      then superioresDelCazadorL nc l1 
                                                      else superioresDelCazadorL nc l2
superioresDelCazadorL nc (Cazador n _ l1 l2 l3)   = if nc == n 
                                                      then [] 
                                                      else if existeElCazador nc l1 
                                                        then n : superioresDelCazadorL nc l1 
                                                        else if existeElCazador nc l2 
                                                          then n : superioresDelCazadorL nc l2 
                                                          else n : superioresDelCazadorL nc l3

-- (Funcion auxiliar) Propósito: dado un nombre de un lobo y un lobo, indica si este lobo existe realmente.
existeElCazador :: Nombre -> Lobo -> Bool
existeElCazador nc (Cria _)                 = False
existeElCazador nc (Explorador _ _ l1 l2)   = existeElCazador nc l1 || existeElCazador nc l2
existeElCazador nc (Cazador n _ l1 l2 l3)   = nc == n || existeElCazador nc l1 || existeElCazador nc l2 || existeElCazador nc l3