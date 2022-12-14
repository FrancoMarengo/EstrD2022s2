-- Números enteros
-- 1
-- a)
sucesor :: Int -> Int
sucesor x = x + 1

-- b)
sumar :: Int -> Int -> Int 
sumar x y = x + y

-- c)
-- PRECOND: 'y' no puede ser igual a 0.
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = (div x y, mod x y)

-- Con mensaje de error (misma precondición)
divisionYResto' :: Int -> Int -> (Int, Int)
divisionYResto' x 0 = error "El divisor no puede ser cero."
divisionYResto' x y = (div x y, mod x y)

-- d)
maxDelPar :: (Int,Int) -> Int
maxDelPar (x, y) = if (x > y) 
                    then x
                    else y

-- 2 
-- sumar (maxDelPar (divisionYResto 49 7)) (sucesor 2)
-- sucesor (maxDelPar (divisionYResto 81 (sumar 6 3)))
-- maxDelPar (divisionYResto 100 (sumar 7 (sucesor 2)))
-- sumar (maxDelPar (-20, -41)) (maxDelPar (divisionYResto (sucesor 149) 5))

-- Tipos enumerativos
-- 1 
data Dir = Norte | Sur | Este | Oeste
           deriving Show

-- a)
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur   = Norte
opuesto Este  = Oeste
opuesto Oeste = Este

-- con case of
opuesto' :: Dir -> Dir
opuesto' d = case d of 
             Norte -> Sur
             Sur   -> Norte
             Oeste -> Este
             Este  -> Oeste

-- b) 
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur     = True
iguales Oeste Oeste = True
iguales Este Este   = True 
iguales _    _      = False

-- c) 
-- PRECOND: El tipo de dato 'Dir' pasado como parámetro no puede ser 'Oeste'
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este  = Sur 
siguiente Sur   = Oeste
siguiente Oeste = error "No existe un tipo de dato 'Dir' siguiente a 'Oeste'."

-- 2 
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
                   deriving Show

-- a) 
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

-- b)
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM _         = False

-- c)
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = (indiceDeDia d1) > (indiceDeDia d2)

indiceDeDia :: DiaDeSemana -> Int
indiceDeDia Lunes     = 1
indiceDeDia Martes    = 2 
indiceDeDia Miercoles = 3 
indiceDeDia Jueves    = 4
indiceDeDia Viernes   = 5
indiceDeDia Sabado    = 6
indiceDeDia Domingo   = 7

-- d)
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes   = False
estaEnElMedio Domingo = False
estaEnElMedio _       = True

-- 3
-- a)
negar :: Bool -> Bool
negar False = True
negar True  = False

-- b)
implica :: Bool -> Bool -> Bool
implica True b = b
implica _    _ = True

-- c)
yTambien :: Bool -> Bool -> Bool
yTambien False _ = False
yTambien True  b = b

-- d)
oBien :: Bool -> Bool -> Bool
oBien False b = b
oBien True  _ = True

-- Registros 
-- 1 
data Persona = P String Int 
              -- Nombre Edad
              deriving Show

nombre :: Persona -> String
nombre (P n e) = n

edad :: Persona -> Int
edad (P n e) = e

crecer :: Persona -> Persona
crecer (P n e) = P n (e+1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre n (P _ e) = P n e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P _ e1) (P _ e2) = e1 > e2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if (esMayorQueLaOtra p1 p2) 
                      then p1
                      else p2

tienenLaMismaEdad :: Persona -> Persona -> Bool
tienenLaMismaEdad (P _ e1) (P _ e2) = e1 == e2

-- 2
data TipoDePokemon = Agua | Fuego | Planta
                     deriving Show
data Entrenador = E String Pokemon Pokemon
                 -- Nombre Pokemon Pokemon
                 deriving Show
data Pokemon = PK TipoDePokemon Int
               -- TipoDePokemon Porcentaje de energía
               deriving Show

superaA :: Pokemon -> Pokemon -> Bool
superaA (PK t1 _) (PK t2 _) = esTipoMasFuerte t1 t2

esTipoMasFuerte :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoMasFuerte Agua Fuego   = True
esTipoMasFuerte Fuego Planta = True
esTipoMasFuerte Planta Agua  = True 
esTipoMasFuerte _      _     = False 

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (E _ p1 p2) = unoSi(esPokemonDeTipo t p1) + unoSi(esPokemonDeTipo t p2)

unoSi :: Bool -> Int
unoSi True = 1
unoSi _    = 0

esPokemonDeTipo :: TipoDePokemon -> Pokemon -> Bool
esPokemonDeTipo t (PK t' _) = sonMismoTipo t t'

sonMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonMismoTipo Fuego Fuego   = True
sonMismoTipo Agua Agua     = True
sonMismoTipo Planta Planta = True
sonMismoTipo _      _      = False

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) = listaDePokemon e1 ++ listaDePokemon e2

listaDePokemon :: Entrenador -> [Pokemon]
listaDePokemon (E _ p1 p2) = p1:p2:[]

-- Funciones Polimórficas
-- 1 
-- a)
loMismo :: a -> a
loMismo x = x

siempreSiete :: a -> Int
siempreSiete x = 7

swap :: (a,b) -> (b, a)
swap (x, y) = (y, x)
-- Las variables son de tipos diferentes ya que, si se le pasa una tupla (Int, String) como parámetro, entonces esperará como retorno
-- una tupla de tipo (String, Int). Si esto no es así, entonces se espera como retorno una tupla (Int, String).

-- 2
-- Estas funciones son polimórficamente paramétricas ya que al momento de llamarlas y pasarle los parámetros, éstos pueden
-- ser de cualquier tipo y la función seguiria funcionando de manera correcta.

-- Pattern matching sobre listas
-- 2
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _  = False

-- 3
-- PRECOND: La lista pasada como parámetro no puede estar vacía.
elPrimero :: [a] -> a
elPrimero []     = error "La lista no puede ser vacia."
elPrimero (x:xs) = x

-- 4
-- PRECOND: La lista pasada como parámetro no puede estar vacía.
sinElPrimero :: [a] -> [a]
sinElPrimero []     = error "La lista no puede ser vacia."
sinElPrimero (x:xs) = xs

-- 5
-- PRECOND: La lista pasada como parámetro no puede estar vacía.
splitHead :: [a] -> (a, [a])
splitHead []     = error "La lista no puede ser vacia."
splitHead (x:xs) = (x, xs)
