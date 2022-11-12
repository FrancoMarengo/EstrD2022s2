#include <iostream>
using namespace std;

struct NodeT;

typedef NodeT* Tree;

// Retorna un árbol vacío.
Tree emptyT();

// Retorna un árbol con el elemento dado en la raíz, los hijos izquierdos y derechos dados.
Tree nodeT(int elem, Tree left, Tree right);

// Indica si el árbol está vacío.
bool isEmptyT(Tree t);

// Retorna el elemento ubicado en la raíz del árbol dado.
int rootT(Tree t);

// Retorna el hijo izquierdo del árbol dado.
Tree left(Tree t);

// Retorna el hijo derecho del árbol dado.
Tree right(Tree t);