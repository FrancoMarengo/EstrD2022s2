#include <iostream>
using namespace std;

#include "DualNet.h"
#include "Ruta.h"
#include "Rutas.h"
#include "Cliente.h"
#include "Clientes.h"
#include "MapCR.h"
#include "Switch.h"
#include "BinHeapC.h"

struct  DNHeaderSt {
  MapCR  mcr;
  Switch sw;
  /* INV.REP.:
      * si un cliente c en el map mrc está asociado a una ruta r
        entonces el switch sw tiene a c conectado en la ruta r
      * si un cliente c está conectado en la ruta r en el switch sw
        entonces c está asociado a r en el map mcr
  */
};

/* Propósito: Retorna un DualNet vacío.
   Eficiencia: O(1).
*/
DualNet emptyDN() {
  DNHeaderSt* dn = new DNHeaderSt;
  dn->mcr = emptyMCR();
  dn->sw = newSwitch();
  return dn;
}

/* Propósito: Retorna la cantidad de clientes conectados en el DualNet dado.
   Eficiencia: O(1).
*/
int cantidadDeClientesConectados(DualNet dn) {
  return sizeMCR(dn->mcr);
}

/* Propósito: Indica si la Ruta dada se encuentra disponible en el DualNet dado.
   Eficiencia: O(r) siendo r la cantidad de rutas disponibles a una distancia, 
                    y siendo esta distancia la longitud de la Ruta dada.
*/
bool estaDisponible(Ruta r, DualNet dn) {
  Rutas rs = disponiblesADistancia(dn->sw, lenRuta(r));
  RutasIterator ri = iniciarRecorridoDeRutas(rs);
  bool disponible = false;
  while(!estaAlFinalDeLasRutas(ri)) {
    disponible |= mismaRuta(r, rutaActual(ri));
    AvanzarASiguienteRuta(ri);
  }
  LiberarRutasIterator(ri);
  return(disponible);
}

/* Propósito: Conecta a un cliente dado en una Ruta dada en el DualNet dado.
   Precond: La Ruta dada debe estar disponible.
   Eficiencia: O(log C) siendo C la cantidad de clientes del DualNet.
*/
void ConectarCliente(Ruta r, Cliente c, DualNet dn) {
  Ruta rutaAnterior = lookupMCR(c, dn->mcr);
  if(rutaAnterior != NULL) {
    Desconectar(rutaAnterior, dn->sw);
  }
  Conectar(c, r, dn->sw);
  AddMCR(c, r, dn->mcr);
}

/* Propósito: Desconecta al Cliente dado del DualNet.
   Eficiencia: O(log C) siendo C la cantidad de clientes del DualNet.
   OBS: Si el Cliente no estaba conectado en el DualNet, entonces no hace nada.
*/
void DesconectarCliente(Cliente c, DualNet dn) {
  Ruta rutaCliente = lookupMCR(c, dn->mcr);
  if(rutaCliente != NULL) {
    Desconectar(rutaCliente, dn->sw);
    DeleteMCR(c, dn->mcr);
  } 
}

/* Propósito: Retorna una BinHeapC con pares pin-Cliente por cada Cliente del DualNet dado y
              cada longitud de Ruta asociada a estos Clientes.
   Eficiencia: O(C log C) siendo C la cantidad de Clientes en el DualNet.
*/
BinHeapC pinPorCliente(DualNet dn) {
  BinHeapC h = emptyHC();
  ClientesIterator csi = iniciarRecorridoClientes(keysMCR(dn->mcr));
  while(!estaAlFinalDeLosClientes(csi)) {
    Cliente cl = clienteActual(csi);
    int pin = lenRuta(lookupMCR(cl, dn->mcr));
    InsertHC(pin, cl, h);
    AvanzarASiguienteCliente(csi);
  }
  LiberarClientesIterator(csi);
  return h;
}

/* Propósito: Libera el DualNet dado de memoria.
   Eficiencia: O(r) siendo r todas los nodos del switch.
*/
void LiberarDN(DualNet dn) {
  LiberarMCR(dn->mcr);
  LiberarSwitch(dn->sw);
  delete dn;
}

void ShowDualNet(DualNet dn) {
  cout << "=======" << endl;
  cout << "DUALNET" << endl;
  cout << "=======" << endl;
  cout << "Map Clientes->Ruta" << endl;
  ShowMapCR(dn->mcr, 2);
  cout << "Switch Clientes" << endl;
  ShowSwitch(dn->sw, 2);
  cout << "=======" << endl;
}

