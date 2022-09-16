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
-- caminoAlTesoro :: Mapa -> Maybe [Dir]
-- caminoAlTesoro (Fin c) = if (hayTesoroEnCofre c)
--                           then (Just [])
--                           else Nothing
-- caminoAlTesoro (Bifurcacion c m1 m2) = if (hayTesoroEnCofre c)
--                                         then (Just [])
--                                         else elegirCamino (caminoAlTesoro m1) (caminoAlTesoro m2)
-- 
-- elegirCamino :: Maybe [Dir] -> Maybe [Dir] -> Maybe [Dir]
-- elegirCamino Nothing   (Just ds) = (Just (Der : ds))
-- elegirCamino (Just ds) Nothing   = (Just (Izq : ds))
-- elegirCamino _        _          = error "No hay un tesoro en el mapa dado."