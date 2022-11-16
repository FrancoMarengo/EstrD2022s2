#include <iostream>
#include <iomanip>
#include <limits.h>
using namespace std;

#include "BinHeapC.h"
#include "Cliente.h"
#include "Clientes.h"

#define MIN_DATA INT_MIN
                 // Macro definida en limits.h
#define MIN_CLIENTE cliente("");

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

BinHeapC emptyHC() { // O(1)
  BinHeapHeaderSt* h = new BinHeapHeaderSt;
  int* pins = new int[16];
  Cliente* clientes = new Cliente[16];
  h->maxSize = 16;
  h->curSize = 0;
  h->pins = pins;
  h->clientes = clientes;
  h->pins[0] = MIN_DATA;
  h->clientes[0] = MIN_CLIENTE;
  return h;
}

bool isEmptyHC(BinHeapC h) {  // O(1)
  return h->curSize == 0;
}

int  findMinPinHC(BinHeapC h) { // O(1)
  return h->pins[1];
}

Cliente findMinClienteHC(BinHeapC h) { // O(1)
  return h->clientes[1];
}

//---------------------------------------
// Auxiliar para ampliar el espacio de elementos de la heap
//  en caso de ser necesario
//---------------------------------------
void AumentarEspacio(BinHeapC h) { // O(N)
  h->maxSize = h->maxSize*2;
  int* tempPns = new int[h->maxSize];
  Cliente* tempCls = new Cliente[h->maxSize];
  tempPns[0] = MIN_DATA;
  tempCls[0] = MIN_CLIENTE;
  for(int i = 1; i <= h->curSize; i++) {
    tempPns[i] = h->pins[i];
    tempCls[i] = h->clientes[i];
  }
  delete h->pins;
  delete h->clientes;
  h->pins = tempPns;
  h->clientes = tempCls;
}

void InsertHC(int pin, Cliente c, BinHeapC h) { // O(log N)
  if(h->curSize==h->maxSize-1) { AumentarEspacio(h); }
  int curNode = ++h->curSize;
  while(pin < h->pins[curNode/2]) {
    h->pins[curNode] = h->pins[curNode/2];
    h->clientes[curNode] = h->clientes[curNode/2];
    curNode /= 2;
  }
  if(pin == h->pins[curNode/2] && esClienteMayor(c, h->clientes[curNode/2])) {
    h->pins[curNode] = h->pins[curNode/2];
    h->clientes[curNode] = h->clientes[curNode/2];
    curNode /= 2;
  }
  h->clientes[curNode] = c;
  h->curSize++;
}

// PRECOND: h->curSize > 0
void DeleteMinHC(BinHeapC h) { // O(log N)
  int child; int curNode;
  int last = h->pins[h->curSize--];
  for(curNode=1; curNode*2 <= h->curSize; curNode=child) {
    child = curNode*2;
    if ((child != h->curSize) && (h->pins[child+1] < h->pins[child])) { 
      child++; 
    }
    if (last > h->pins[child]) { 
      h->pins[curNode] = h->pins[child]; 
    }
    else { break; }
  }
  h->pins[curNode] = last;
}

void LiberarHC(BinHeapC h) { // O(1)
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

