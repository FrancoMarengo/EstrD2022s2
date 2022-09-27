module Stack (
    Stack,
    emptyS,
    isEmptyS,
    push,
    top,
    pop,
    lenS
)

where

data Stack a = S [a] Int
 {- INV.REP.: en (S xs n)
     * n indica la cantidad de elementos en xs. 
 -}

-- O(1)
-- Crea una pila vacía.
emptyS :: Stack a
emptyS = S [] 0

-- O(1) 
-- Dada una pila indica si está vacía.
isEmptyS :: Stack a -> Bool
isEmptyS (S xs n) = n == 0

-- O(1)
-- Dados un elemento y una pila, agrega el elemento a la pila.
push :: a -> Stack a -> Stack a
push x (S xs n) = (S (x:xs) (n+1))

-- O(n) siendo n el costo operacional de head
-- Dada un pila devuelve el elemento del tope de la pila.
top :: Stack a -> a
top (S xs n) = head xs

-- O(n) siendo n el costo operacional de la funcion tail.
-- Dada una pila devuelve la pila sin el primer elemento.
pop :: Stack a -> Stack a
pop (S xs n) = (S (tail xs) (n-1))

-- Dada la cantidad de elementos en la pila.
-- Costo: constante
lenS :: Stack a -> Int
lenS (S xs n) = n
