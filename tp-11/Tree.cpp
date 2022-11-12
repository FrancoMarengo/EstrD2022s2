#include <iostream>
#include "Tree.h"
using namespace std;

struct NodeT {
int elem;
NodeT* left;
NodeT* right;
};

// Retorna un árbol vacío.
Tree emptyT() {
    return NULL;
}

// Retorna un árbol con el elemento dado en la raíz, los hijos izquierdos y derechos dados.
Tree nodeT(int elem, Tree left, Tree right) {
    NodeT* t = new NodeT;
    t->elem = elem;
    t->left = left;
    t->right = right;
    return t;
}

// Indica si el árbol está vacío.
bool isEmptyT(Tree t) {
    return t == NULL;
}

// Retorna el elemento ubicado en la raíz del árbol dado.
int rootT(Tree t) {
    return t->elem;
}

// Retorna el hijo izquierdo del árbol dado.
Tree left(Tree t) {
    return t->left;
}

// Retorna el hijo derecho del árbol dado.
Tree right(Tree t) {
    return t->right;
}
