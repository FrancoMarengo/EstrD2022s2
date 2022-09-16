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