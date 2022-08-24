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