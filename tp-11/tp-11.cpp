#include <iostream>
#include "LinkedList.h"
#include "Set.h"
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


int main() {
    Set s = emptyS();
    AddS(2, s);
    RemoveS(2, s);
    AddS(4, s);
    LinkedList xs = setToList(s);
    cout << head(xs) << endl;
}

