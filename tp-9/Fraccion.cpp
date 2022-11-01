#include <iostream>
#include "Fraccion.h"

// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador) {
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return f;
}

// Propósito: devuelve el numerador
int numerador(Fraccion f) {
    return f.numerador;
}

// Propósito: devuelve el denominador
int denominador(Fraccion f) {
    return f.denominador;
}

// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f) {
    return f.numerador / f.denominador;
}

// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2) {
    Fraccion r;
    r.numerador = f1.numerador * f2.numerador;
    r.denominador = f1.denominador * f2.denominador;
    return r;
}

// Propósito: devuelve el maximo comun divisor entre dos números
int mcd(int num1, int num2) {
    int mcd = 0;
    int a = max(num1, num2);
    int b = min(num1, num2);
    do {
        mcd = b;
        b = a%b;
        a = mcd;
    } while(b!=0);
    return mcd;
}

// Propósito: retorna el minimo comun múltiplo entre dos números
int mcm(int num1, int num2) {
    int mcm = 0;
    int a = std::max(num1, num2);
    int b = std::min(num1, num2);
    mcm = (a/mcd(a,b))*b;
    return mcm;
}

// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
Fraccion simplificada(Fraccion p) {
    int maxcd = mcd(p.numerador, p.denominador);
    p.numerador = p.numerador / maxcd;
    p.denominador = p.denominador / maxcd;
    return p;
}

// Propósito: devuelve la primera componente
Fraccion sumF(Fraccion f1, Fraccion f2) {
    Fraccion r;
    if (f1.denominador != f2.denominador) {
        int mincm = mcm(f1.denominador, f2.denominador);
        r.denominador = mincm;
        int s1 = (mincm / f1.denominador) * f1.numerador;
        int s2 = (mincm / f2.denominador) * f2.numerador;
        r.numerador = s1 + s2;
    } else {
        r.numerador = f1.numerador + f2.numerador;
        r.denominador = f1.denominador;
    }
    return r;
}