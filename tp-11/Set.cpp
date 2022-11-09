#include <iostream>
#include "Set.h"
using namespace std;

struct NodoS {
    int elem; // valor del nodo
    NodoS* siguiente; // puntero al siguiente nodo
};

struct SetSt {
    int cantidad; // cantidad de elementos diferentes
    NodoS* primero; // puntero al primer nodo
};

/* INV.REP.:
    * No se pueden repetir los elem entre nodos.
    * La cantidad del SetSt indica la cantidad de nodos en el Set.
*/

// Crea un conjunto vacío.
// Costo: O(1)
Set emptyS() {
    SetSt* s = new SetSt;
    s->cantidad = 0;
    s->primero = NULL;
    return s;
}

// Indica si el conjunto está vacío.
// Costo: O(1)
bool isEmptyS(Set s) {
    return s->cantidad == 0;
}

// Indica si el elemento pertenece al conjunto.
// Costo: O(n) en peor caso, siendo n la cantidad de elementos en el set.
bool belongsS(int x, Set s) {
    NodoS* nodoAMirar = s->primero;
    bool belongs = false;
    while (nodoAMirar != NULL && !belongs) {
        belongs |= nodoAMirar->elem == x;
        nodoAMirar = nodoAMirar->siguiente;
    }
    return belongs;
}

// Agrega un elemento al conjunto.
// Costo: O(n) en peor caso recorre todos los elementos en el set con belongsS.
void AddS(int x, Set s) {
    if (!belongsS(x, s)) {
        NodoS* nuevoN = new NodoS;
        nuevoN->elem = x;
        nuevoN->siguiente = s->primero;
        s->primero = nuevoN;
        s->cantidad++;
    }
}

// Quita un elemento dado.
// Costo: O(n) en peor caso, recorre todos los elementos en el set 2 veces.
void RemoveS(int x, Set s) {
    if (belongsS(x, s)) {
        if (s->primero->elem == x) {
            NodoS* temp = s->primero->siguiente;
            delete s->primero;
            s->primero = temp;
        } else {
            NodoS* nodoAMirar = s->primero;
            NodoS* anteriorN = s->primero;
            while (nodoAMirar->elem != x) {
                anteriorN = nodoAMirar;
                nodoAMirar = nodoAMirar->siguiente;
            }
            anteriorN->siguiente = nodoAMirar->siguiente;
            delete nodoAMirar;
        }
        s->cantidad--;
    }
}

// Devuelve la cantidad de elementos.
// Costo: O(1)
int sizeS(Set s) {
    return s->cantidad;
}

// Devuelve una lista con los lementos del conjunto.
// Costo: O(n) siempre recorre todos los elementos del set.
LinkedList setToList(Set s) {
    NodoS* nodoAMirar = s->primero;
    LinkedList xs = nil();
    while (nodoAMirar != NULL) {
        Snoc(nodoAMirar->elem, xs);
        nodoAMirar = nodoAMirar->siguiente;
    }
    return xs;
}

// Libera la memoria ocupada por el conjunto.
// Costo: O(n) siempre recorre todos los elementos del set para borrarlos.
void DestroyS(Set s) {
    if (s->cantidad > 0) {
        NodoS* temp = s->primero->siguiente;
        while (temp != NULL) {
            delete s->primero;
            s->primero = temp;
            temp = temp->siguiente;
        }
        delete s->primero;
    }
    delete s;
}

