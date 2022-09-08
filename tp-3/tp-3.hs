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

