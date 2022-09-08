-- Tipos recursivos simples
-- 1.1
data Color = Azul | Rojo
           deriving Show
data Celda = Bolita Color Celda | CeldaVacia
           deriving Show

-- Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
-- existe una operación sobre listas que ayude a resolver el problema.
nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia       = 0
nroBolitas c (Bolita col cel) = unoSi(sonMismoColor c col) + nroBolitas c cel

-- (Funcion auxiliar) Dado un booleano indica 1 si es True o 0 si es False.
unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

-- (Funcion auxiliar) Dado dos colores indica si son el mismo color
sonMismoColor :: Color -> Color -> Bool
sonMismoColor Azul Azul = True
sonMismoColor Rojo Rojo = True
sonMismoColor _    _    = False

-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner c cel = (Bolita c cel)

-- Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de Gobstones, esta función es total.
sacar :: Color -> Celda -> Celda
sacar c CeldaVacia       = CeldaVacia
sacar c (Bolita col cel) = if sonMismoColor c col
                            then cel 
                            else (Bolita col (sacar c cel))

-- Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 c cel = cel
ponerN n c cel = (Bolita c (ponerN (n-1) c cel))

-- 1.2
data Objeto = Cacharro | Tesoro
            deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
            deriving Show

-- Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino -> Bool
hayTesoro Fin           = False
hayTesoro (Nada c)      = False || hayTesoro c
hayTesoro (Cofre obs c) = hayTesoro' obs || hayTesoro c

-- (Funcion auxiliar) Dada una lista de objetos indica si hay al menos un tesoro.
hayTesoro' :: [Objeto] -> Bool
hayTesoro' []       = False
hayTesoro' (ob:obs) = esTesoro ob || hayTesoro' obs

-- (Funcion auxiliar) Dado un objeto indica si es tesoro
esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

-- Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
-- Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
-- Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin           = error "No hay ningun tesoro en este camino."
pasosHastaTesoro (Nada c)      = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre obs c) = if hayTesoro' obs 
                                  then 0
                                  else 1 + pasosHastaTesoro c

-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
-- pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 c             = hayTesoroEnTramo c
hayTesoroEn n (Fin)         = False
hayTesoroEn n (Nada c)      = hayTesoroEn (n-1) c
hayTesoroEn n (Cofre obs c) = hayTesoroEn (n-1) c

--(Funcion auxiliar) Indica si hay al menos un tesoro en el tramo de camino indicado.
hayTesoroEnTramo :: Camino -> Bool
hayTesoroEnTramo (Cofre obs _) = hayTesoro' obs
hayTesoroEnTramo _             = False 

-- Indica si hay al menos “n” tesoros en el camino.
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 _             = True
alMenosNTesoros n (Fin)         = False
alMenosNTesoros n (Nada c)      = alMenosNTesoros n c
alMenosNTesoros n (Cofre obs c) = 
    let tesoros = cantTesorosEn obs in
        tesoros >= n || alMenosNTesoros (n - tesoros) c 

-- (Funcion auxiliar) Retorna la cantidad de tesoros en una lista de objetos.
cantTesorosEn :: [Objeto] -> Int
cantTesorosEn []       = 0
cantTesorosEn (ob:obs) = unoSi(esTesoro ob) + cantTesorosEn obs

-- Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si el rango es 3 y 5, 
-- indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están incluidos tanto 3 como 5 en el resultado.
cantTesorosEntre :: Int -> Int -> Camino -> Int -- Sin terminar
cantTesorosEntre 0 n2 c (Fin)         = 0
cantTesorosEntre 0 n2 c (Nada c)      = 0
cantTesorosEntre 0 n2 c (Cofre obs c) = 0
cantTesorosEntre n1 0 c (Fin)         = 0
cantTesorosEntre n1 0 c (Nada c)      = 0
cantTesorosEntre n1 0 c (Cofre obs c) = 0
cantTesorosEntre n1 n2 (Fin)          = cantTesorosEntre (n1-1) (n2-1) c
cantTesorosEntre n1 n2 (Nada c)       = cantTesorosEntre (n1-1) (n2-1) c
cantTesorosEntre n1 n2 (Cofre obs c)  = cantTesorosEntre (n1-1) (n2-1) c
