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
-- sucesor (maxDelPar (divisionYResto 81 (sumar 6)))
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
vieneDespues Lunes     _ = False
vieneDespues Martes    d = vieneAntesDeMartes d
vieneDespues Miercoles d = vieneAntesDeMiercoles d
vieneDespues Jueves    d = vieneAntesDeJueves d
vieneDespues Viernes   d = vieneAntesDeViernes d 
vieneDespues Sabado    d = vieneAntesDeSabado d
vieneDespues Domingo   d = vieneAntesDeDomingo d

vieneAntesDeMartes :: DiaDeSemana -> Bool
vieneAntesDeMartes Lunes = True
vieneAntesDeMartes _     = False

vieneAntesDeMiercoles :: DiaDeSemana -> Bool
vieneAntesDeMiercoles Lunes  = True
vieneAntesDeMiercoles Martes = True
vieneAntesDeMiercoles _      = False

vieneAntesDeJueves :: DiaDeSemana -> Bool
vieneAntesDeJueves Jueves  = False
vieneAntesDeJueves Viernes = False
vieneAntesDeJueves Sabado  = False
vieneAntesDeJueves Domingo = False
vieneAntesDeJueves _       = True

vieneAntesDeViernes :: DiaDeSemana -> Bool
vieneAntesDeViernes Viernes = False
vieneAntesDeViernes Sabado  = False
vieneAntesDeViernes Domingo = False
vieneAntesDeViernes _       = True

vieneAntesDeSabado :: DiaDeSemana -> Bool
vieneAntesDeSabado Sabado  = False
vieneAntesDeSabado Domingo = False
vieneAntesDeSabado _       = True

vieneAntesDeDomingo :: DiaDeSemana -> Bool
vieneAntesDeDomingo Domingo = False
vieneAntesDeDomingo _       = True

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
implica True False = False
implica _    _     = True

-- c)
yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _    _    = False

-- d)
oBien :: Bool -> Bool -> Bool
oBien True _    = True
oBien _    True = True
oBien _    _    = False