#include <iostream>
#include "Persona.h"
#include "Pokemon.h"
#include "Entrenador.h"
#include "ArrayList.h"
using namespace std;

// Registros
// Ejercicio 1
// Definir el tipo de dato Persona, como un puntero a un registro con el nombre y la edad de la
// persona.

// Definido en Persona.h y Persona.cpp

// Ejercicio 2
// Modelaremos los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta,
// sinónimo de string) y un porcentaje de energía (que inicia en 100); y Entrenador, como un
// nombre, una cantidad de pokémon y un array de pokémon.

// Definido en Pokemon.h, Pokemon.cpp, Entrenador.h y Entrenador.cpp

// Array Lists
// Ejercicio 3

// Definido en ArrayList.h y ArrayList.cpp

// Ejercicio 4
// Definir las siguientes funciones utilizando la interfaz de ArrayList:

// Devuelve la suma de todos los elementos.
int sumatoria(ArrayList xs) {
    int sum = 0;
    for(int i = 0; lengthAL(xs) > i; i++) {
        sum += get(i, xs);
    }
    return sum;
}

// Incrementa en uno todos los elementos.
void sucesores(ArrayList xs) {
    for(int i = 0; lengthAL(xs) > i; i++) {
        set(i, get(i, xs) + 1, xs);
    }
}

// Indica si el elemento pertenece a la lista.
bool pertenece(int x, ArrayList xs) {
    bool pertenece = false;
    for(int i = 0; lengthAL(xs) > i; i++) {
        pertenece = pertenece || x == get(i, xs);
    }
    return pertenece;
}

// Indica la cantidad de elementos iguales a x.
int apariciones(int x, ArrayList xs) {
    int ap = 0;
    for(int i = 0; lengthAL(xs) > i; i++) {
        if (x == get(i, xs)) {
            ap++;
        }
    }
    return ap;
}

// Crea una nueva lista a partir de la primera y la segunda (en ese orden).
ArrayList append(ArrayList xs, ArrayList ys) {
    ArrayList ap = newArrayListWith(lengthAL(xs) + lengthAL(ys));
    for(int i = 0; lengthAL(xs) > i; i++) {
        add(get(i, xs), ap);
    }
    for(int i = 0; lengthAL(ys) > i; i++) {
        add(get(i, ys), ap);
    }
    return ap;
}

// Devuelve el elemento más chico de la lista.
int minimo(ArrayList xs) {
    int minHastaAhora = get(0 , xs);
    for (int i = 1; lengthAL(xs) > i; i++) {
        if (get(i, xs) < minHastaAhora) {
            minHastaAhora = get(i, xs);
        }
    }
    return minHastaAhora;
}