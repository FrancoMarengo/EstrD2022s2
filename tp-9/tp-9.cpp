#include <iostream>
using namespace std;
#include "Par.h"
#include "Fraccion.h"

// Ejercicio 2
// Indicar el propósito de los siguientes procedimientos o funciones, dando algunos ejemplos de uso
// junto con su resultado. Considerar el consumo de memoria de cada programa, y si puede mejorarse.

// 1.
/* Propósito: imprime en consola la posición del caracter c1 hasta la posición del caracter c2,
              teniendo en cuenta el sistema de codificacion de caracteres utilizado y sus posiciones.
   Ejemplo: printFromTo('a', 'f') imprime en consola "97, 98, 99, 100, 101, 102,"
   Memoria: Reserva un stack frame para el procedimiento, con un espacio en memoria para la variable 'i'
            que posteriormente se va modificando hasta completar el while, buen uso de memoria.
*/
// Precondición: c1 < c2
void printFromTo(char c1, char c2) {
    for(int i = 0; c1 + i <= c2; i++) {
        cout << c1 + i << ", ";
    }
    cout << endl;
}

// 2.
/* Propósito: retorna el fibonacci del número n dado.
   Observaciones: realiza el fibonacci de manera inversa.
   Ejemplo: fc(5) retorna el número 120.
   Memoria: reserva un stack frame para el procedimiento, con un espacio en memoria para n y x, los cuales
            se van modificando hasta completar el while, buen uso de memoria.
*/
// Precondición: n >= 0
int fc(int n) {
    int x = 1;
    while(n > 0) {
        x = x * n;
        n--;
    }
    return x;
}

//3.
/*
   Propósito: retorna la suma de los números desde n hasta m.
   Observaciones: realiza la suma con recursión.
   Ejemplo: ft(2,5) retorna el número 14, que es igual a 2+3+4+5.
   Memoria: reserva un stack frame para el procedimiento, con un espacio en memoria para n y m, pero por cada
            vez que se ejecuta el while, reserva otro stack frame para el mismo procedimiento, debido al llamado
            recursivo de la misma función, implementación mejorada en ftIterativa.
*/
// Precondición: n <= m
int ft(int n, int m) {
    if (n == m) {
        return n;
    }
    return n + ft(n+1, m);
}

/*
   Propósito: retorna la suma de los números desde n hasta m.
   Observaciones: realiza la suma con recursión.
   Ejemplo: ft(2,5) retorna el número 14, que es igual a 2+3+4+5.
   Memoria: reserva un stack frame para el procedimiento, con un espacio en memoria para n, m e i, que se modifican
            por cada vez que se ejecuta el while, buen manejo de memoria.
*/
// Precondición: n <= m
int ftIterativa(int n, int m) {
    int i = n;
    while(i < m) {
        n += i+1;
        i++;
    }
    return n;
}

// Ejercicio 3
// Dada la estructura de pares representada como struct en C++, definir las siguientes funciones
// sobre pares. Recordar probar las implementaciones en un procedimiento main.

// DEFINIDAS COMO TAD en "Par.h" y "Par.cpp".

// Ejercicio 4
// Dar dos implementaciones para las siguientes funciones, una iterativa y otra recursiva, y utilizando
// la menor cantidad posible de variables. Recordar definir subtareas en caso de que sea estrictamente
// necesario.

// 1.Iterativa
// Propósito: imprime n veces un string s. 
void printN(int n, string s) {
    while (n > 0) {
        cout << s << " ";
        n--;
    }
}

// 1.Recursiva
// Propósito: imprime n veces un string s. 
void printNR(int n, string s) {
    if (n > 0) {
        cout << s << " ";
        printNR((n-1),s);   
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// 2.ITerativa 
// Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
// Precondición: n >= 0 (una cuenta regresiva desde un número negativo lleva a infinito negativo)
void cuentaRegresivaR(int n) {
    while (n != 0) {
        cout << n << endl;
        n--;
    }
}

// 2.Recursiva 
// Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
// Precondición: n >= 0 (una cuenta regresiva desde un número negativo lleva a infinito negativo)
void cuentaRegresiva(int n) {
    if (n > 0) {
        cout << n << endl;
        cuentaRegresiva(n-1);
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Iterativa
// Propósito: imprime los números de 0 hasta un n positivo, separados por saltos de línea.
// Precondición: n >= 0;
void desdeCeroHastaNPositivo(int n) {
    int i = 0;
    while (n >= i) {
        cout << i << endl;
        i++;
    }
}

// Iterativa
// Propósito: imprime los números de 0 hasta un n negativo, separados por saltos de línea.
// Precondición: n < 0
void desdeCeroHastaNNegativo(int n) {
    int i = 0;
    while (n <= i) {
        cout << i << endl;
        i--;
    }
}

// 3.Iterativa
// Propósito: imprime los números de 0 hasta n, separados por saltos de línea. 
void desdeCeroHastaN(int n) {
    if (n >= 0) {
        desdeCeroHastaNPositivo(n);
    } else if (n < 0) {
        desdeCeroHastaNNegativo(n);
    }
}

// Propósito: imprime los números de n hasta m, separados por saltos de línea
// Precondición: m >= 0
void desdeNHastaNPositivo(int n, int m) {
    if (n <= m) {
        cout << n << endl;
        desdeNHastaNPositivo(n+1,m);
    }
}

// Propósito: imprime los números de n hasta m, separados por saltos de línea
// Precondición: m < 0
void desdeNHastaNNegativo(int n, int m) {
    if (n >= m) {
        cout << n << endl;
        desdeNHastaNNegativo(n-1,m);
    }
}

// 3.Recursiva
// Propósito: imprime los números de 0 hasta n, separados por saltos de línea. 
void desdeCeroHastaNR(int n) {
    if (n >= 0) {
        desdeNHastaNPositivo(0, n);
    } else if (n < 0) {
        desdeNHastaNNegativo(0, n);
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


// Iterativa
// Propósito: realiza la multiplicación entre dos números.
// Precond: m >= 0
int multPos(int n, int m) {
    int x = 0;
    while (m > 0) {
        x += n;
        m--;
    }
    return x;
}

// Iterativa
// Propósito: realiza la multiplicación entre dos números.
// Precond: m <= 0
int multNeg(int n, int m) {
    int x = 0;
    while (m < 0) {
        x -= n;
        m++;
    }
    return x; 
}

// 4.Iterativa
// Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++). 
int mult(int n, int m) {
     if (m < 0) {
        return multNeg(n, m);
    } else {
        return multPos(n, m);
    }
}

// 4.Recursiva
// Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
int multR(int n, int m) {
     if (m < 0) {
        n += n;
        return mult(n, (m+1));
    } else if (m > 0) {
        n += n;
        return mult(n, (m-1)); 
    } else {
        return 0;
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


// 5.Iterativa
// Propósito: imprime los primeros n char del string s, separados por un salto de línea.
// Precondición: el string tiene al menos n char.
void primerosN(int n, string s) {
    for(int i = 0; i < n; i++) {
        cout << s[i] << endl;
    }
}

// Recursiva
// Propósito: imprime desde n hasta m char del string s, separados por un salto de linea
// Precond: n <= m 
void desdeNHastaN(int n, int m, string s) {
    if (n < m) {
        cout << s[n] << endl;
        desdeNHastaN((n+1), m, s);
    }
}

// 5.Recursiva
// Propósito: imprime los primeros n char del string s, separados por un salto de línea.
// Precondición: el string tiene al menos n char.
void primerosNR(int n, string s) {
    desdeNHastaN(0,n,s);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// 6.Iterativa
// Propósito: indica si un char c aparece en el string s.
bool pertenece(char c, string s) {
    bool p = false;
    int i  = 0;
    while(!p && s[i] != 0) {
        p = (c == s[i]);
        i++;
    }
    return p;
}

// 6.Recursiva
// Propósito: indica si un char c aparece en el string s.
bool perteneceR(char c, string s) {
    if (s[0] != 0) {
        return c == s[0] || perteneceR(c, s.substr(1));
    } else {
        return false;
    }
}

// 7.Iterativa
// Propósito: devuelve la cantidad de apariciones de un char c en el string s.
int apariciones(char c, string s) {
    int i = 0;
    int cant = 0;
    while(s[i] != 0) {
        if (s[i] == c) {
            cant++;
        }
        i++;
    }
    return cant;
}

// 7.Recursiva
// Propósito: devuelve la cantidad de apariciones de un char c en el string s.
int aparicionesR(char c, string s) {
    if (s[0] == 0) {
        return 0;
    } else if (s[0] == c) {
        return 1 + aparicionesR(c, s.substr(1));
    } else {
        return aparicionesR(c, s.substr(1));
    }
}

// Ejercicio 5
// Dada la estructura de fracciones representada como struct en C++, definir las siguientes funciones
// sobre fracciones. Recordar probar las implementaciones en un procedimiento main.

int main() {
    Fraccion f = consFraccion(28,6);
    Fraccion r = consFraccion(15, 4);
    Fraccion res = sumF(f, r);
    cout << numerador(res) << endl;
    cout << denominador(res) << endl;
}