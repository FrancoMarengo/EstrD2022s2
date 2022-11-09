#include <iostream>
#include "LinkedList.h"
using namespace std;

struct SetSt;

typedef SetSt* Set;

// Crea un conjunto vacío.
Set emptyS();

// Indica si el conjunto está vacío.
bool isEmptyS(Set s);

// Indica si el elemento pertenece al conjunto.
bool belongsS(int x, Set s);

// Agrega un elemento al conjunto.
void AddS(int x, Set s);

// Quita un elemento dado.
void RemoveS(int x, Set s);

// Devuelve la cantidad de elementos.
int sizeS(Set s);

// Devuelve una lista con los lementos del conjunto.
LinkedList setToList(Set s);

// Libera la memoria ocupada por el conjunto.
void DestroyS(Set s);

