#include <iostream>
#include "LinkedList.h"
#include "Set.h"
#include "Queue.h"
#include "Tree.h"
#include "ArrayList.h"
using namespace std;

// Linked Lists
// Ejercicio 1

// Implementado en LinkedList.h y LinkedList.cpp

// Ejercicio 2
// Definir las siguientes funciones utilizando la interfaz de LinkedList, indicando costos:

// Devuelve la suma de todos los elementos.
// Costo: O(n) siendo n la cantidad de elementos de xs.
int sumatoria(LinkedList xs) {
    ListIterator ixs = getIterator(xs);
    int sum = current(ixs);
    Next(ixs);
    while (!atEnd(ixs)) {
        sum += current(ixs);
        Next(ixs);
    }
    DisposeIterator(ixs);
    return sum;
}

// Incrementa en uno todos los elementos.
// Costo: O(n) siendo n la cantidad de elementos de xs.
void Sucesores(LinkedList xs) {
    ListIterator ixs = getIterator(xs);
    while (!atEnd(ixs)) {
        SetCurrent((current(ixs)+1), ixs);
        Next(ixs);
    }
    DisposeIterator(ixs);
}

// Indica si el elemento pertenece a la lista.
// Costo: O(n) en peor caso, siendo n la cantidad de elementos de xs.
bool pertenece(int x, LinkedList xs) {
    ListIterator ixs = getIterator(xs);
    bool pertenece = false;
    while (!atEnd(ixs)) {
        pertenece |= current(ixs) == x;
        Next(ixs);
    }
    DisposeIterator(ixs);
    return pertenece;
}

// Indica la cantidad de elementos iguales a x.
// Costo: O(n) siendo n la cantidad de elementos de xs.
int apariciones(int x, LinkedList xs) {
    ListIterator ixs = getIterator(xs);
    int apariciones = 0;
    while (!atEnd(ixs)) {
        if (current(ixs) == x) {
            apariciones++;
        }
        Next(ixs);
    }
    DisposeIterator(ixs);
    return apariciones;
}

// Devuelve el elemento más chico de la lista.
// Costo: O(n) siendo n la cantidad de elementos de xs.
int minimo(LinkedList xs) {
    ListIterator ixs = getIterator(xs);
    int minimoHastaAhora = current(ixs);
    Next(ixs);
    while (!atEnd(ixs)) {
        if (minimoHastaAhora > current(ixs)) {
            minimoHastaAhora = current(ixs);
        }
        Next(ixs);
    }
    DisposeIterator(ixs);
    return minimoHastaAhora;
}

// Dada una lista genera otra con los mismos elementos, en el mismo orden.
// Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo? (Guardando el último
// elemento en la estructura de LinkedList)
// Costo: O(n) siendo n la cantidad de elementos de xs.
LinkedList copy(LinkedList xs) {
    ListIterator ixs = getIterator(xs);
    LinkedList   ys  = nil();
    while (!atEnd(ixs)) {
        Snoc(current(ixs), ys);
        Next(ixs);
    }
    DisposeIterator(ixs);
    return ys;
}

// Agrega todos los elementos de la segunda lista al final de los de la primera.
// La segunda lista se destruye.
// Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo? (Guardando el último
// elemento en la estructura de LinkedList)
// Costo: O(n * m) siendo n la cantidad de elementos de xs y m la cantidad de elementos de ys.
// void Append(LinkedList xs, LinkedList ys) {
//     ListIterator iys = getIterator(ys);
//     while (!atEnd(iys)) {
//         Snoc(current(iys), xs);
//         Next(iys);
//     }
//     DisposeIterator(iys);
//     DestroyL(ys);
// }

// Ejercicio 3
// Agregar la operación de Append a la interfaz de LinkedList, e implementarla como implementador
// en O(1).

// Set
// Ejercicio 4

// Resuelto en Set.h y Set.cpp

// Queue
// Ejercicio 5

// Resuelto en Queue.h y Queue.cpp

// Arboles binarios 
// Ejercicio 6

// Dado un árbol binario de enteros devuelve la suma entre sus elementos.
int sumarT(Tree t) {
    int sum = 0;
    if (!isEmptyT(t)) {
        sum += rootT(t);
        sum += sumarT(left(t));
        sum += sumarT(right(t));
    }
    return sum;
}

// Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
// en inglés).
int sizeT(Tree t) {
    int s = 0;
    if (isEmptyT(t)) {
        s += 1 + sizeT(left(t)) + sizeT(right(t));
    }
    return s;
}

// Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
// árbol.
bool perteneceT(int e, Tree t) {
    bool pertenece = false;
    if (!isEmptyT(t)) {
        pertenece |= (rootT(t) == e)        
                  || perteneceT(e, left(t)) 
                  || perteneceT(e, right(t));
    }
    return pertenece;
}

// (funcion auxiliar)
// Retorna 1 si el booleano dado es true o 0 si es false.
int unoSi(bool b) {
    if (b) {
        return 1;
    } else {
        return 0;
    }
}

// Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
// iguales a e.
int aparicionesT(int e, Tree t) {
    int ap = 0;
    if (!isEmptyT(t)) {
        ap += unoSi(rootT(t) == e) + aparicionesT(e, left(t))
                                   + aparicionesT(e, right(t));
    }
    return ap;
}

// Dado un árbol devuelve su altura.
int heightT(Tree t) {
    if (heightT(left(t)) > heightT(right(t))) {
        return 1 + heightT(left(t));
    } else {
        return 1 + heightT(right(t));
    }
}

// Dado un árbol devuelve una lista con todos sus elementos.
ArrayList toList(Tree t) {
    // Preguntar si no conviene usar la interfaz de LinkedList para hacer merge y
    // para tener una interfaz útil que no genere memory leaks.
    return newArrayList();
}

// Dado un árbol devuelve los elementos que se encuentran en sus hojas.
ArrayList leaves(Tree t) {
    if (!isEmptyT(t) && isEmptyT(left(t)) && isEmptyT(right(t))) {
        ArrayList al = newArrayList();
        add(rootT(t), al);
        return al;
    } else if (!isEmptyT(t)) {
    //    return merge(leaves(left(t)), leaves(right(t)));
    }
}

// Dados un número n y un árbol devuelve una lista con los nodos de nivel n
ArrayList levelN(int n, Tree t) {
    return newArrayList();
}


int main() {
    
}

