#include <iostream>
#include "Entrenador.h"
using namespace std;

struct EntrenadorSt {
    string   nombre;
    Pokemon* pokemon;
    int      cantPokemon;
};

// Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese tamaño, devuelve
// un entrenador.
Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon) {
    EntrenadorSt* ent = new EntrenadorSt;
    ent->nombre       = nombre;
    ent->pokemon      = pokemon;
    ent->cantPokemon  = cantidad;
    return ent;
}

// Devuelve el nombre del entrenador.
string nombreDeEntrenador(Entrenador e) {
    return e->nombre;
}

// Devuelve la cantidad de pokémon que posee el entrenador.
int cantidadDePokemon(Entrenador e) {
    return e->cantPokemon;
}

// Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e) {
    int cant = 0;
    Pokemon* ps = e->pokemon;
    for(int i = 0; i < e->cantPokemon; i++) {
        if(tipoDePokemon(ps[i]) == tipo) {
            cant++;
        }
    }
    return cant;
}

// Devuelve el pokémon número i de los pokémon del entrenador.
// Precondición: existen al menos i − 1 pokémon.
Pokemon pokemonNro(int i, Entrenador e) {
    return e->pokemon[i-1];
}

// Dado un pokemon y un entrenador, indica si, para cada pokemon del entrenador, el pokemon
// dado les gana.
bool pokLeGanaATodos(Pokemon p, Entrenador e) {
    bool leGanaATodos = true;
    Pokemon* ps = e->pokemon;
    for(int i = 0; i < e->cantPokemon; i++) {
        leGanaATodos = leGanaATodos && superaA(p, ps[i]);
    }
    return leGanaATodos;
}

// Dados dos entrenadores, indica si, para cada pokémon del segundo entrenador, el primero
// posee al menos un pokémon que le gane.
bool leGanaATodos(Entrenador e1, Entrenador e2) {
    bool alMenosUnoGana = false;
    Pokemon* ps = e1->pokemon;
    for(int i = 0; i < e1->cantPokemon; i++) {
        alMenosUnoGana = alMenosUnoGana || pokLeGanaATodos(ps[i], e2);
    }
    return alMenosUnoGana;
}

