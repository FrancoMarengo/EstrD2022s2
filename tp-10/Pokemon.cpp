#include <iostream>
#include "Pokemon.h"
using namespace std;

struct PokeSt {
    TipoDePokemon tipo;
    int vida;
};

// Dado un tipo devuelve un pokémon con 100 % de energía.
Pokemon consPokemon(TipoDePokemon tipo) {
    PokeSt* p = new PokeSt;
    p->tipo  = tipo;
    p-> vida = 100;
    return p;
}

// Devuelve el tipo de un pokémon.
TipoDePokemon tipoDePokemon(Pokemon p) {
    return p->tipo;
}

// Devuelve el porcentaje de energía.
int energia(Pokemon p) {
    return p->vida;
}

// Le resta energía al pokémon.
void perderEnergia(int energia, Pokemon p) {
    p->vida -= energia;
}

// Funcion auxiliar (No es parte de la interfaz)
// Dados dos tipos de pokemon indica si el primero es superior al segundo. Agua supera
// a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
bool tipoSuperaATipo(TipoDePokemon t1, TipoDePokemon t2) {
    return (t1 == "Agua"   && t2 == "Fuego")  ||
           (t1 == "Fuego"  && t2 == "Planta") ||
           (t1 == "Planta" && t2 == "Agua"); 
}

// Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
// a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
bool superaA(Pokemon p1, Pokemon p2) {
    return tipoSuperaATipo(p1->tipo, p2->tipo);
}
