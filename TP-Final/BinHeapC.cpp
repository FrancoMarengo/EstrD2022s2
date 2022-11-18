#include <iostream>
#include <iomanip>
#include <limits.h>
using namespace std;

#include "BinHeapC.h"
#include "Cliente.h"
#include "Clientes.h"

#define MIN_DATA INT_MIN
                 // Macro definida en limits.h

struct BinHeapHeaderSt{
  int      maxSize;
  int      curSize;
  int*     pins;
  Cliente* clientes;
  /* INV.REP.
      * 0 <= curSize < maxSize
      * los arrays pins y clientes tienen la misma cantidad de celdas reservadas,
        y maxSize guarda esa cantidad
      * los elementos válidos de ambos arrays se encuentran entre las posiciones 
        0 y curSize
      * pins[0] guarda el valor más chico posible de ser almacenado        
      * el array pins guarda los elementos según el orden dado por un árbol binario
        completo que mantiene el invariante de Heap

    OBS:
      * los valores (pins[i], clientes[i]) corresponden a los diferentes elementos,
        para 1<=i<=curSize
      * se usará el nombre N como sinónimo de curSize
  */
};

/* Propósito: Retorna una BinHeapC vacía.
   OBS: Se crea con un array de 17 posiciones, pero con 16 posiciones disponibles,
        ya que en la posición 0 de ambos ArrayLists se ubican centinelas.
   Eficiencia: O(1)
*/
BinHeapC emptyHC() {
  BinHeapHeaderSt* h = new BinHeapHeaderSt;
  int* pins = new int[17];
  Cliente* clientes = new Cliente[17];
  h->maxSize = 16;
  h->curSize = 0;
  h->pins = pins;
  h->clientes = clientes;
  h->pins[0] = MIN_DATA;
  h->clientes[0] = NULL;
  return h;
}

/* Propósito: Indica si el BinHeapC dado está vacío.
   Eficiencia: O(1)
*/
bool isEmptyHC(BinHeapC h) { 
  return h->curSize == 0;
}

/* Propósito: Retorna el mínimo pin del BinHeapC dado.
   Precond: h->curSize > 0.
   Eficiencia: O(1)
*/
int  findMinPinHC(BinHeapC h) {
  return h->pins[1];
}

/* Propósito: Retorna el mínimo Cliente del BinHeapC dado.
   Precond: h->curSize > 0.
   Eficiencia: O(1)
*/
Cliente findMinClienteHC(BinHeapC h) {
  return h->clientes[1];
}

//---------------------------------------
// Auxiliar para ampliar el espacio de elementos de la heap
//  en caso de ser necesario
//---------------------------------------

/* Propósito: Aumenta el espacio de la BinHeapC dada.
   OBS: El espacio se duplica.
   Eficiencia: O(N)
*/
void AumentarEspacio(BinHeapC h) {
  h->maxSize = h->maxSize*2;
  int* tempPns = new int[h->maxSize];
  Cliente* tempCls = new Cliente[h->maxSize];
  tempPns[0] = MIN_DATA;
  tempCls[0] = NULL;
  for(int i = 1; i <= h->curSize; i++) {
    tempPns[i] = h->pins[i];
    tempCls[i] = h->clientes[i];
  }
  delete h->pins;
  delete h->clientes;
  h->pins = tempPns;
  h->clientes = tempCls;
}

/* Propósito: Inserta un par pin-Cliente dado en la BinHeapC dada.
   Eficiencia: O(log N)
*/
void InsertHC(int pin, Cliente c, BinHeapC h) {
  if(h->curSize==h->maxSize-1) { AumentarEspacio(h); }  // Aumenta el espacio en caso de necesitarlo.
  int curNode = h->curSize + 1;
  while(pin < h->pins[curNode/2] || (pin == h->pins[curNode/2] && esClienteMayor(h->clientes[curNode/2], c))) {
    h->pins[curNode] = h->pins[curNode/2];          // Flota el par pin-cliente dado en sus respectivos arrays
    h->clientes[curNode] = h->clientes[curNode/2];  // siempre y cuando sea menor al padre.
    curNode /= 2;
  }
  h->pins[curNode] = pin;  
  h->clientes[curNode] = c;
  h->curSize++;  // Aumenta en uno el curSize, ya que se agrega un elemento.
}

/* Propósito: Borra el mínimo elemento de la BinHeapC dada.
   Precond: h->curSize > 0.
   Eficiencia: O(log N)
*/
void DeleteMinHC(BinHeapC h) {
  int child; int curNode;
  int last = h->pins[h->curSize--];  // Guarda el ultimo elemento y resta el curSize en uno.
  for(curNode=1; curNode*2 <= h->curSize; curNode=child) {
    child = curNode*2;  // Apunta al hijo izquierdo del curNode
    if (((child != h->curSize) && (h->pins[child+1] < h->pins[child]))
    || ((h->pins[child+1] == h->pins[child]) && esClienteMayor(h->clientes[child], h->clientes[child+1]))) { 
      child++;  // Si el hijo derecho de curNode es menor al hijo izquierdo, entonces apunta al hijo derecho.
    }
    if (last > h->pins[child] 
    || (last == h->pins[child] && esClienteMayor(h->clientes[h->curSize], h->clientes[child]))) { 
      h->pins[curNode] = h->pins[child];          // Si el hijo elegido es menor al ultimo elemento
      h->clientes[curNode] = h->clientes[child];  // entonces hunde al ultimo a la posicion del hijo.
    } else { break; }
  }
  h->pins[curNode] = last;  // Cuando el ultimo no es menor a ninguno de los hijos entonces deja de hundirlo.
  h->clientes[curNode] = h->clientes[h->curSize];
}

/* Propósito: Libera la BinHeapC de memoria.
   Eficiencia: O(1)
*/
void LiberarHC(BinHeapC h) {
  delete h->clientes;
  delete h->pins;
  delete h;
}

//---------------------------------------
// Auxiliar para el mostrado
//---------------------------------------
void PadHC(int offset) {
  for(int i=0; i<offset; i++) {
    cout << " ";
  }
}

void ShowHC(BinHeapC h, int offset) {
  PadHC(offset); cout << "Heap[" << h->curSize << "," << h->maxSize << "] {" << endl;
  if (h->curSize>0) {
    PadHC(offset);
    cout << "  (" << setw(2) << h->pins[1] << ", " << nombre(h->clientes[1]) << ")" << endl;
  }
  for (int i=2;i<=h->curSize;i++) {
    PadHC(offset);
    cout << ", (" << setw(2) << h->pins[i] << ", " << nombre(h->clientes[i]) << ")" << endl;
  }
  if (h->curSize>0) { PadHC(offset); } else { cout << " "; }; cout << "}" << endl;
}

