#include <iostream>
#include "LinkedList.h"
using namespace std;

// Linked Lists
// Ejercicio 1

// Implementado en LinkedList.h y LinkedList.cpp

// Ejercicio 2
// Definir las siguientes funciones utilizando la interfaz de LinkedList, indicando costos:

// Devuelve la suma de todos los elementos.
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
void Sucesores(LinkedList xs) {
    ListIterator ixs = getIterator(xs);
    while (!atEnd(ixs)) {
        SetCurrent((current(ixs)+1), ixs);
        Next(ixs);
    }
    DisposeIterator(ixs);
}

// Indica si el elemento pertenece a la lista.
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
LinkedList copy(LinkedList xs) {
    ListIterator ixs = getIterator(xs);
    LinkedList   ys  =  nil();
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
void Append(LinkedList xs, LinkedList ys) {
    ListIterator iys = getIterator(ys);
    while (!atEnd(iys)) {
        Snoc(current(iys), xs);
        Next(iys);
    }
    DisposeIterator(iys);
    DestroyL(ys);
}

int main() {
    LinkedList xs = nil();
    Snoc(7, xs);
    Snoc(9, xs);
    Tail(xs);
    cout << head(xs) << endl;
}

