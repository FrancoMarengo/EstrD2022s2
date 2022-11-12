#include <iostream>
#include "Queue.h"
using namespace std;

struct NodoQ {
    int elem;         // valor del nodo
    NodoQ* siguiente; // puntero al siguiente nodo
};

struct QueueSt {
    int cantidad;   // cantidad de elementos
    NodoQ* primero; // puntero al primer nodo
    NodoQ* ultimo;  // puntero al ultimo nodo
};

// Crea una lista vacía.
// Costo: O(1).
Queue emptyQ() {
    QueueSt* q  = new QueueSt;
    q->cantidad = 0;
    q->primero  = NULL;
    q->ultimo   = NULL;
    return q;
}

// Indica si la lista está vacía.
// Costo: O(1).
bool isEmptyQ(Queue q) {
    return q->cantidad == 0;
}

// Devuelve el primer elemento.
// Costo: O(1).
int firstQ(Queue q) {
    return q->primero->elem;
}

// Agrega un elemento al final de la cola.
// Costo: O(1).
void Enqueue(int x, Queue q) {
    NodoQ* nuevoN = new NodoQ;
    nuevoN->elem = x;
    nuevoN->siguiente = NULL; 
    if (q->cantidad > 0) {
        q->ultimo->siguiente = nuevoN;
        q->ultimo = nuevoN;
    } else {
        q->primero = nuevoN;
        q->ultimo = nuevoN;
    }
    q->cantidad++;
}

// Quita el primer elemento de la cola.
// Costo: O(1).
void Dequeue(Queue q) {
    NodoQ* temp = q->primero->siguiente;
    delete q->primero;
    q->primero = temp;
    q->cantidad--;
}

// Devuelve la cantidad de elementos de la cola.
// Costo: O(1).
int lengthQ(Queue q) {
    return q->cantidad;
}

// Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
// Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
// Costo: O(1).
void MergeQ(Queue q1, Queue q2) {
    if (q1->cantidad > 0) {
        q1->ultimo->siguiente = q2->primero;
        q1->ultimo = q2->ultimo;
    } else {
        q1->primero = q2->primero;
        q1->ultimo = q2->ultimo;
    }
    q1->cantidad = q1->cantidad + q2->cantidad;
    delete q2;
}

// Libera la memoria ocupada por la lista.
// Costo: O(n).
void DestroyQ(Queue q) {
    if (q->cantidad > 0) {
        NodoQ* temp = q->primero->siguiente;
        while (temp != NULL) {
            delete q->primero;
            q->primero = temp;
        }
        delete q->primero;
    }
    delete q;
}
