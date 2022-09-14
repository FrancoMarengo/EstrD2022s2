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
-- PRECOND: La cantidad "n" de tesoros requeridos no puede ser negativa.
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
-- PRECOND: i < j.
cantTesorosEntre :: Int -> Int -> Camino -> Int 
cantTesorosEntre i j (Fin)         = 0
cantTesorosEntre 0 j c             = cantTesorosHasta j c 
cantTesorosEntre i j (Nada c)      = cantTesorosEntre (i-1) (j-1) c 
cantTesorosEntre i j (Cofre obs c) = cantTesorosEntre (i-1) (j-1) c

cantTesorosHasta :: Int -> Camino -> Int
cantTesorosHasta 0 c             = cantTesorosEnTramo c 
cantTesorosHasta i (Fin)         = 0
cantTesorosHasta i (Nada c)      = cantTesorosHasta (i-1) c 
cantTesorosHasta i (Cofre obs c) = cantTesorosEn obs + cantTesorosHasta (i-1) c 

cantTesorosEnTramo :: Camino -> Int 
cantTesorosEnTramo (Fin)         = 0
cantTesorosEnTramo (Nada _)      = 0
cantTesorosEnTramo (Cofre obs _) = cantTesorosEn obs 

-- Tipos arbóreos
-- 2.1
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
            deriving Show

-- 1
-- Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

-- 2
-- Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT _ t1 t2) = 1 + sizeT t1 + sizeT t2

-- 3
-- Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n t1 t2) = (NodeT (n*2) (mapDobleT t1) (mapDobleT t2))

-- 4
-- Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x EmptyT          = False 
perteneceT x (NodeT y t1 t2) = x == y || perteneceT x t1 || perteneceT x t2 

-- 5
-- Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT x EmptyT          = 0
aparicionesT x (NodeT y t1 t2) = unoSi(x==y) + (aparicionesT x t1) + (aparicionesT x t2)

-- 6
-- Dado un árbol devuelve los elementos que se encuentran en sus hojas.
leaves :: Tree a -> [a]
leaves (NodeT x EmptyT EmptyT) = [x]
leaves (NodeT _ t1 t2)         = leaves t1 ++ leaves t2 

-- 7
-- Dado un árbol devuelve su altura.
heightT :: Tree a -> Int
heightT EmptyT          = 0 
heightT (NodeT _ t1 t2) = 1 + max (heightT t1) (heightT t2)

-- 8
-- Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho, en cada nodo del árbol.
mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x t1 t2) = (NodeT x (mirrorT t2) (mirrorT t1)) 

-- 9
-- Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
toList :: Tree a -> [a]
toList EmptyT          = []
toList (NodeT x t1 t2) = (agregarAlFinal (toList t1) x) ++ (toList t2)

-- Funcion auxiliar practica 2.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e     = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

-- 10 
-- Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un nodo es
--  la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
-- distancia de la raiz a uno de sus hijos es 1.
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT          = []
levelN 0 (NodeT x t1 t2) = [x]
levelN n (NodeT x t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

-- 11
-- Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : zipListas (listPerLevel t1) (listPerLevel t2)

-- (Funcion auxiliar) Dadas dos listas de listas, hace un append de la lista en posición n de la primer lista con la lista en posición n
-- de la segunda lista.
zipListas :: [[a]] -> [[a]] -> [[a]]
zipListas []  yss           = yss
zipListas xss []            = xss
zipListas (xs:xss) (ys:yss) = (xs++ys) : zipListas xss yss

-- 12
-- Devuelve los elementos de la rama más larga del árbol
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x t1 t2) = if (heightT t1 > heightT t2)
                                then x : ramaMasLarga t1
                                else x : ramaMasLarga t2

-- 13
-- Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas.
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT                  = []
todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]] 
todosLosCaminos (NodeT x t1 t2)         = (consATodasLasListas x (todosLosCaminos t1)) ++ (consATodasLasListas x (todosLosCaminos t2))

-- (Funcion auxiliar) Dado un elemento y una lista de listas, agrega el elemento a todas las listas dentro de la lista.
consATodasLasListas :: a -> [[a]] -> [[a]]
consATodasLasListas x []       = []
consATodasLasListas x (ys:yss) = (x : ys) : (consATodasLasListas x yss)

-- Expresiones aritméticas
data ExpA = Valor Int
          | Sum ExpA ExpA
          | Prod ExpA ExpA
          | Neg ExpA    
          deriving Show

-- 1
-- Dada una expresión aritmética devuelve el resultado evaluarla.
eval :: ExpA -> Int
eval (Valor n)    = n
eval (Sum e1 e2)  = (eval e1) + (eval e2)
eval (Prod e1 e2) = (eval e1) * (eval e2)
eval (Neg e1)     = (eval e1) * (-1) 

-- 2
{- 
Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando notación matemática convencional):
a) 0 + x = x + 0 = x
b) 0 * x = x * 0 = 0
c) 1 * x = x * 1 = x
d) - (- x) = x
   Neg (Neg (Valor 2) = (Valor 2)
-}
simplificar :: ExpA -> ExpA
simplificar (Sum e1 e2)  = simplificarSum e1 e2
simplificar (Prod e1 e2) = simplificarProd e1 e2
simplificar (Neg e1)     = simplificarNeg (Neg e1)
simplificar e            = e

-- (Funcion auxiliar) Dadas dos expresiones aritméticas a sumar, simplifica la suma según el siguiente criterios:
-- a) 0 + x = x + 0 = x
simplificarSum :: ExpA -> ExpA -> ExpA
simplificarSum e1 e2 = if (eval e1 == 0)
                        then e2
                        else if (eval e2 == 0)
                         then e1
                         else (Sum e1 e2) 

-- (Funcion auxiliar) Dadas dos expresiones aritméticas a multiplicar, simplifica el producto según los siguientes criterios:
-- b) 0 * x = x * 0 = 0
-- c) 1 * x = x * 1 = x
simplificarProd :: ExpA -> ExpA -> ExpA 
simplificarProd e1 e2 = if (eval e1 == 0 || eval e2 == 0)
                         then (Valor 0)
                          else if (eval e1 == 1)
                           then e2
                           else if (eval e2 == 1)
                            then e1
                            else (Prod e1 e2) 

-- (Funcion auxiliar) Dada una expresion aritmética a negar, simplifica la negación segun el siguiente criterio:
-- d) - (- x) = x
simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg (Neg e)) = e  
<<<<<<< HEAD




=======
                    
>>>>>>> 4b54340e02803cad7b8fbad4ff7f0b3825977e64
