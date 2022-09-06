-- Recursion sobre listas
-- 1
-- Dada una lista de enteros devuelve la suma de todos sus elementos
sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (n:ns) = n + sumatoria ns 

-- 2
-- Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad de elementos que posee.
longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

-- 3
-- Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (n:ns) = (n+1) : sucesores ns

-- 4
-- Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion :: [Bool] -> Bool
conjuncion []     = True 
conjuncion (b:bs) = b && conjuncion bs

-- 5
-- Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion :: [Bool] -> Bool
disyuncion []     = False
disyuncion (b:bs) = b || disyuncion bs

-- 6
-- Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]] -> [a]
aplanar []     = []
aplanar (x:xs) = x ++ aplanar xs

-- 7
-- Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece :: Eq a => a -> [a] -> Bool
pertenece e []     = False 
pertenece e (x:xs) = e == x || pertenece e xs

-- 8
-- Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a -> [a] -> Int
apariciones e []     = 0
apariciones e (x:xs) = if (e == x)
                        then 1 + apariciones e xs
                        else apariciones e xs

-- 9
-- Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n []     = []
losMenoresA n (x:xs) = if (n > x)
                        then x : losMenoresA n xs
                        else losMenoresA n xs

-- 10
-- Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n []     = []
lasDeLongitudMayorA n (x:xs) = if ((longitud x) > n)
                                then x : lasDeLongitudMayorA n xs
                                else lasDeLongitudMayorA n xs

-- 11
-- Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la lista.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e     = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

-- 12 
-- Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los elementos de la segunda a continuación. 
-- definida en Haskell como (++).
agregar :: [a] -> [a] -> [a]
agregar [] ys     = ys
agregar (x:xs) ys = x : agregar xs ys 

-- 13
-- Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida en Haskell como reverse.
reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = agregarAlFinal (reversa xs) x 

-- 14
-- Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el máximo entre el elemento n
-- de la primera lista y de la segunda lista, teniendo en cuenta que las listas no necesariamente tienen la misma longitud.
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos ns []            = ns
zipMaximos [] ns2           = ns2
zipMaximos (n:ns) (n2:ns2)  = maxDelPar(n, n2) : zipMaximos ns ns2

-- Funcion auxiliar de practica 1.
maxDelPar :: (Int,Int) -> Int
maxDelPar (x, y) = if (x > y) 
                    then x
                    else y

-- 15
-- Dada una lista devuelve el mínimo
-- PRECOND: La lista no puede ser vacía.
elMinimo :: Ord a => [a] -> a
elMinimo []     = error "La lista no puede ser vacia."
elMinimo (x:[]) = x
elMinimo (x:xs) = minimo x (elMinimo xs)

-- (Funcion auxiliar) Dado dos elementos devuelve el mínimo.
minimo :: Ord a => a -> a -> a
minimo x y = if (x > y)
              then y
              else x

-- Recursion sobre numeros
-- 1
-- Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta llegar a 0. 
-- Si n es 0 devuelve 1. La función es parcial si n es negativo.
-- PRECOND: El número n pasado por parámetro no puede ser negativo.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- 2 
-- Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre n y 1 (incluidos).
-- Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = if (n<0)
                     then []
                     else n : cuentaRegresiva (n-1)

-- 3
-- Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e = e : repetir (n-1) e

-- 4
-- Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs. Si la lista es vacía, devuelve una lista vacía.
losPrimeros :: Int -> [a] -> [a]
losPrimeros n []     = []
losPrimeros 0 _      = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

-- 5
-- Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros n []     = []
sinLosPrimeros 0 xs     = xs
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs

-- Registros
-- 1
data Persona = P String Int
              -- Nombre Edad
              deriving Show

-- Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n []     = []
mayoresA n (p:ps) = if ((edad p) > n)
                     then p : mayoresA n ps
                     else mayoresA n ps

-- Funcion auxiliar de práctica 1.
edad :: Persona -> Int
edad (P n e) = e

-- Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona.
promedioEdad :: [Persona] -> Int
promedioEdad [] = error "La lista no puede ser vacia."
promedioEdad ps = div (sumatoriaEdades ps) (longitud ps)

-- (Funcion auxiliar) Dada una lista de personas, devuelve la sumatoria de sus edades.
sumatoriaEdades :: [Persona] -> Int
sumatoriaEdades []     = 0
sumatoriaEdades (p:ps) = edad p + sumatoriaEdades ps

-- Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la lista al menos posee una persona.
elMasViejo :: [Persona] -> Persona
elMasViejo (p:[]) = p
elMasViejo (p:ps) = laQueEsMayor p (elMasViejo ps)

-- Funcion auxiliar de práctica 1.
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if (esMayorQueLaOtra p1 p2) 
                      then p1
                      else p2

-- Funcion auxiliar de la práctica 1.
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P _ e1) (P _ e2) = e1 > e2

-- 2 
data TipoDePokemon = Agua | Fuego | Planta
                     deriving Show
data Pokemon = ConsPokemon TipoDePokemon Int
               deriving Show
data Entrenador = ConsEntrenador String [Pokemon]
                  deriving Show

-- Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ ps) = longitud ps

-- Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ ps) = cantPokemonDe' t ps

-- (Funcion auxiliar) Devuelve la cantidad de Pokémon de determinado tipo en una lista de Pokémon.
cantPokemonDe' :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonDe' t []     = 0
cantPokemonDe' t (p:ps) = unoSi(sonMismoTipo t (tipoDePokemon p)) + cantPokemonDe' t ps

-- (Funcion auxiliar) Retorna el tipo de un Pokémon.
tipoDePokemon :: Pokemon -> TipoDePokemon
tipoDePokemon (ConsPokemon t _) = t

-- Funcion auxiliar de práctica 1.
sonMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonMismoTipo Fuego Fuego   = True
sonMismoTipo Agua Agua     = True
sonMismoTipo Planta Planta = True
sonMismoTipo _      _      = False

-- Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían a los Pokemon del segundo entrenador.
losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan t (ConsEntrenador _ ps1) (ConsEntrenador _ ps2) = losQueLeGanan' t ps1 ps2

-- (Funcion auxiliar) Dados dos listas de Pokemon, indica la cantidad de Pokemon de cierto tipo,
-- que le ganarian a los Pokemon de la segunda lista.
losQueLeGanan' :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
losQueLeGanan' t []     _   = 0
losQueLeGanan' t _      []  = 0
losQueLeGanan' t (p:ps) ps' = unoSi ((sonMismoTipo t (tipoDePokemon p)) && superaATodos p ps') + losQueLeGanan' t ps ps'

-- (Funcion auxiliar) Dado un Bool, retorna 1 si es True, o 0 si es False.
unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

-- (Funcion auxiliar) Indica si un Pokemon supera a TODOS los Pokemon de una lista dada.
superaATodos :: Pokemon -> [Pokemon] -> Bool
superaATodos p' []     = True
superaATodos p' (p:ps) = superaA p' p && superaATodos p' ps

-- Funcion auxiliar de práctica 1.
superaA :: Pokemon -> Pokemon -> Bool
superaA (ConsPokemon t1 _) (ConsPokemon t2 _) = esTipoMasFuerte t1 t2

-- Funcion auxiliar de práctica 1.
esTipoMasFuerte :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoMasFuerte Agua Fuego   = True
esTipoMasFuerte Fuego Planta = True
esTipoMasFuerte Planta Agua  = True 
esTipoMasFuerte _      _     = False

-- Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ ps) = hayTipo Fuego ps && hayTipo Agua ps && hayTipo Planta ps

-- (Funcion auxiliar) Dada una lista de Pokémon y un TipoDePokemon, retorna True si hay al menos un Pokémon del tipo indicado.
hayTipo :: TipoDePokemon -> [Pokemon] -> Bool
hayTipo t []     = False
hayTipo t (p:ps) = sonMismoTipo t (tipoDePokemon p) || hayTipo t ps

-- 3
data Seniority = Junior | SemiSenior | Senior
                 deriving Show
data Proyecto = ConsProyecto String
                deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
           deriving Show
data Empresa = ConsEmpresa [Rol]
               deriving Show

-- Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = proyectos' rs

-- (Funcion auxiliar) Dada una lista de roles denota una lista de proyectos en los que trabajan los roles, sin elementos repetidos.
proyectos' :: [Rol] -> [Proyecto]
proyectos' []     = []
proyectos' (r:rs) = agregarProyectoSiNoEsta (proyectoDeRol r) (proyectos' rs)

-- (Funcion auxiliar) Agrega a la lista un elemento si no esta presente en la lista.
agregarProyectoSiNoEsta :: Proyecto -> [Proyecto] -> [Proyecto]
agregarProyectoSiNoEsta p [] = [p]
agregarProyectoSiNoEsta p ps = if (estaProyectoEnLaLista p ps)
                                     then ps
                                     else p : ps

-- (Funcion auxiliar) Indica si un elemento se encuentra presente en una lista.
estaProyectoEnLaLista :: Proyecto -> [Proyecto] -> Bool
estaProyectoEnLaLista p' []     = False
estaProyectoEnLaLista p' (p:ps) = (sonMismoProyecto p' p) || (estaProyectoEnLaLista p' ps)

-- (Funcion auxiliar) Indica si dos proyectos son el mismo.
sonMismoProyecto :: Proyecto -> Proyecto -> Bool
sonMismoProyecto (ConsProyecto n) (ConsProyecto n') = n == n'

-- (Funcion auxiliar) Retorna el proyecto de un rol.
proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer _ p)  = p 
proyectoDeRol (Management _ p) = p

-- Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
-- además a los proyectos dados por parámetro.
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) ps = losDevSenior' rs ps 

-- (Funcion auxiliar) Dada una lista de roles indica la cantidad de desarrolladores senior que hay,
-- que pertenecen a alguno de los proyectos dados por parámetro.
losDevSenior' :: [Rol] -> [Proyecto] -> Int
losDevSenior' []     ps = 0
losDevSenior' (r:rs) ps = unoSi ((esDevSenior r) && (perteneceAProyectos r ps)) + losDevSenior' rs ps

-- (Funcion auxiliar) Indica si un rol es senior.
esDevSenior :: Rol -> Bool
esDevSenior (Developer Senior _)  = True 
esDevSenior _                     = False

-- (Funcion auxiliar) Indica si un rol pertenece a alguno de los proyectos dados.
perteneceAProyectos :: Rol -> [Proyecto] -> Bool
perteneceAProyectos _ []      = False
perteneceAProyectos r (p:ps)  = perteneceAProyecto r p || perteneceAProyectos r ps 

-- (Funcion auxiliar) Indica si un rol pertenece al proyecto dado.
perteneceAProyecto :: Rol -> Proyecto -> Bool
perteneceAProyecto (Developer _ p) p'  = sonMismoProyecto p p'
perteneceAProyecto (Management _ p) p' = sonMismoProyecto p p'

-- Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (ConsEmpresa rs) = cantQueTrabajanEn' ps rs

-- (Funcion auxiliar) Indica la cantidad de roles que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn' :: [Proyecto] -> [Rol] -> Int
cantQueTrabajanEn' ps []     = 0 
cantQueTrabajanEn' ps (r:rs) = unoSi (perteneceAProyectos r ps) + cantQueTrabajanEn' ps rs 

-- Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su 
-- cantidad de personas involucradas.
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = asignadosPorProyecto' rs

-- (Funcion auxiliar) Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su 
-- cantidad de personas involucradas.
asignadosPorProyecto' :: [Rol] -> [(Proyecto, Int)]
asignadosPorProyecto' []     = []
asignadosPorProyecto' (r:rs) = agregarProyectoATuplas (proyectoDeRol r) (asignadosPorProyecto' rs)

agregarProyectoATuplas :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)]
agregarProyectoATuplas p []           = [(p, 1)]
agregarProyectoATuplas p ((p', n):ts) = if (sonMismoProyecto p p')
                                         then ((p', (n+1)):ts)
                                         else (p',n) : agregarProyectoATuplas p ts
