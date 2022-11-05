#include <iostream>
#include "ArrayList.h"
using namespace std;

struct ArrayListSt {
    int  cantidad;  // cantidad de elementos
    int* elementos; // array de elementos
    int  capacidad; // tamaño del array
};

// Crea una lista con 0 elementos.
// Nota: empezar el array list con capacidad 16.
ArrayList newArrayList() {
    ArrayListSt* xs = new ArrayListSt;
    xs->cantidad = 0;
    xs->capacidad = 16;
    int* es = new int[16];
    xs->elementos = es;
    return xs;
}

// Crea una lista con 0 elementos y una capacidad dada por parámetro.
ArrayList newArrayListWith(int capacidad) {
    if (capacidad > 0) {
        ArrayListSt* xs = new ArrayListSt;
        xs->cantidad = 0;
        xs->capacidad = capacidad;
        int* es = new int[capacidad];
        xs->elementos = es;
        return xs;
    } else {
        ArrayList rs = newArrayList();
        return rs;
    }
}

// Devuelve la cantidad de elementos existentes.
int lengthAL(ArrayList xs) {
    return xs->cantidad;
}

// Devuelve el iésimo elemento de la lista.
int get(int i, ArrayList xs) {
    return xs->elementos[i];
}

// Reemplaza el iésimo elemento por otro dado.
void set(int i, int x, ArrayList xs) {
    if (xs->capacidad > i) {
        xs->elementos[i] = x;
    }    
}

// Decrementa o aumenta la capacidad del array.
// Nota: en caso de decrementarla, se pierden los elementos del final de la lista.
void resize(int capacidad, ArrayList xs) {
        int* temp = new int[capacidad];
        int i     = 0;
        while(i < xs->cantidad && i < capacidad) {
            temp[i] = xs->elementos[i];
            i++;
        }
        delete xs->elementos;
        xs->cantidad  = i;
        xs->elementos = temp;    
}

// Agrega un elemento al final de la lista.
void add(int x, ArrayList xs) {
    if (xs->capacidad == xs->cantidad) {
        resize(xs->capacidad*2, xs);
    } 
    xs->elementos[xs->cantidad] = x;
    xs->cantidad++;
}

// Borra el último elemento de la lista.
void remove(ArrayList xs) {
    if (xs->cantidad > 0) {
        xs->cantidad--;
    }
}