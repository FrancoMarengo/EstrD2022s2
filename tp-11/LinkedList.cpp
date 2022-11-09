#include <iostream>
#include "LinkedList.h"
using namespace std;

struct NodoL {
    int elem;         // valor del nodo
    NodoL* siguiente; // puntero al siguiente nodo
};

// INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer
// desde primero por siguiente hasta alcanzar a NULL
struct LinkedListSt {
    int cantidad;   // cantidad de elementos
    NodoL* primero; // puntero al primer nodo
};

struct IteratorSt {
    NodoL* current;
};

// Crea una lista vacía.
LinkedList nil() {
    LinkedListSt* res = new LinkedListSt;
    res->cantidad = 0;
    res->primero = NULL;
    return res;
}

// Indica si la lista está vacía.
bool isEmpty(LinkedList xs) {
    return xs->cantidad == 0;
}

// Devuelve el primer elemento.
int head(LinkedList xs) {
    return xs->primero->elem;
}

// Agrega un elemento al principio de la lista.
void Cons(int x, LinkedList xs) {
    NodoL* temp = new NodoL;
    temp->elem = x;
    temp->siguiente = xs->primero;
    xs->primero = temp;
    xs->cantidad++;
}

// Quita el primer elemento.
void Tail(LinkedList xs) {
    NodoL* temp = xs->primero->siguiente;
    delete xs->primero;
    xs->primero = temp;
}

// Devuelve la cantidad de elementos.
int length(LinkedList xs) {
    return xs->cantidad;
}

// Agrega un elemento al final de la lista.
void Snoc(int x, LinkedList xs) {
    NodoL* temp = new NodoL;
    temp->elem = x;
    temp->siguiente = NULL;
    if (xs->cantidad > 0) {
        NodoL* proximoNodo = xs->primero;
        while (proximoNodo->siguiente != NULL) {
            proximoNodo = proximoNodo->siguiente;
        }
        proximoNodo->siguiente = temp;
    } else {
        xs->primero = temp;
    }
    xs->cantidad++;
}

// Apunta el recorrido al primer elemento.
ListIterator getIterator(LinkedList xs) {
    IteratorSt* ixs = new IteratorSt;
    ixs->current = xs->primero;
    return ixs; 
}

// Devuelve el elemento actual en el recorrido.
int current(ListIterator ixs) {
    return ixs->current->elem;
}

// Reemplaza el elemento actual por otro elemento.
void SetCurrent(int x, ListIterator ixs) {
    ixs->current->elem = x;
}

// Pasa al siguiente elemento.
void Next(ListIterator ixs) {
    ixs->current = ixs->current->siguiente;
}

// Indica si el recorrido ha terminado.
bool atEnd(ListIterator ixs) {
    return ixs->current == NULL;
}

// Libera la memoria ocupada por el iterador.
void DisposeIterator(ListIterator ixs) {
    delete ixs;
}

// Libera la memoria ocupada por la lista.
void DestroyL(LinkedList xs) {
    if (xs->cantidad > 0) {
        NodoL* temp = xs->primero->siguiente;
        while (xs->primero != NULL) {
            delete xs->primero;
            xs->primero = temp; 
        }
    }
    delete xs;
}