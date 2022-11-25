#include <iostream>
using namespace std;

#include "Switch.h"
#include "Ruta.h"
#include "Rutas.h"
#include "Cliente.h"
#include "Clientes.h"
struct SNode {
  Cliente conexion; // OBS: si es NULL, está disponible.
  SNode*  boca1;
  SNode*  boca2;
  // INV.REP.:
  //  * (BONUS) uno de los 3 campos es distinto de NULL
};

struct  SwHeaderSt {
  SNode* root;
};

//------------------------------------------------------------
// ESTRUCTURA AUXILIAR COLA DE SIGUIENTES PARA RECORRER LINEALMENTE EL SWITCH
//------------------------------------------------------------

struct QNodeSw {
   SNode* node;
   Ruta   ruta;
};

struct INodeSw {
   QNodeSw* elem;
   INodeSw* next; 
};

struct NextsQueueStSw {
    INodeSw* first;
    INodeSw* last;
    // INV.REP.: o ambos son NULL o ambos son distintos de NULL
};
typedef NextsQueueStSw* NextsQueueSw;        // INV.REP.: el puntero NO es NULL

NextsQueueSw emptyQSw() {
  NextsQueueStSw* newQ = new NextsQueueStSw;
  newQ->first = NULL;
  newQ->last  = NULL;  
  return (newQ);
}

bool isEmptyQSw(NextsQueueSw q) {
  return (q->first==NULL);
}

void EnqueueQSw(NextsQueueSw q, Ruta r, SNode* s) {
  INodeSw* newIN = new INodeSw;
  QNodeSw* newQN = new QNodeSw;
  newQN->node = s;
  newQN->ruta  = r;
  newIN->elem  = newQN;
  newIN->next  = NULL;
  if (q->last==NULL) { q->first      = newIN; }
  else               { q->last->next = newIN; }
  q->last = newIN;
}

QNodeSw* DequeueFirstQSw(NextsQueueSw q) {
    // PRECOND: q->first no es NULL
    INodeSw* in = q->first;
    QNodeSw* qn = in->elem;
    q->first = q->first->next;
    if (q->first == NULL) { q->last = NULL; }
    delete(in);
    return(qn);
}

void LiberarQSw(NextsQueueSw q) {
  INodeSw* temp;
  while(! (q->first==NULL)) {
    temp = q->first;
    q->first = q->first->next;
    delete(temp);
  }
  delete(q);
}

//------------------------------------------------------------
// FUNCIONES DE INTERFAZ DE SWITCH.
//------------------------------------------------------------

/* Propósito: Retorna un Switch vacío.
   Eficiencia: O(1).
*/
Switch newSwitch() {
  SwHeaderSt* sw = new SwHeaderSt;
  sw->root = NULL;
  return (sw);
}

/* Propósito: Conecta al Cliente dado a la Ruta dada en el Switch dado.
   Precond: La Ruta dada debe estar disponible en el Switch dado.
   Eficiencia: O(log R) siendo R los elementos del Switch dado.
*/
void Conectar(Cliente c, Ruta r, Switch s) {
  RutaIterator ri = iniciarRuta(r);
  if(s->root == NULL) {  // Caso en que root sea NULL.
    SNode* newN = new SNode;
    newN->conexion = NULL;
    newN->boca1    = NULL;
    newN->boca2    = NULL;
    s->root = newN;
  }
  SNode* currentN = s->root;
  while(!estaAlFinalDeLaRuta(ri)) {  // Recorre la ruta y si se deben agregar 
    if(bocaActual(ri) == Boca1) {        // SNode lo hace.
      if(currentN->boca1 == NULL) {
        SNode* newN = new SNode;
        newN->conexion = NULL;
        newN->boca1    = NULL;
        newN->boca2    = NULL;
        currentN->boca1 = newN; 
      }
      currentN = currentN->boca1;
    } else if (bocaActual(ri) == Boca2) {
      if(currentN->boca2 == NULL) {
        SNode* newN = new SNode;
        newN->conexion = NULL;
        newN->boca1    = NULL;
        newN->boca2    = NULL;
        currentN->boca2 = newN;
      }
      currentN = currentN->boca2;
    }
    AvanzarEnRuta(ri);
  }
  if(currentN->conexion != NULL) {
    cout << "Error: La conexión en la ruta dada no estaba disponible." << endl;
    exit(1);
  } else {
    currentN->conexion = c;
  }
  LiberarRutaIterator(ri);
}

// FUNCION AUXILIAR
/* Propósito: Verifica que el SNode en la ruta dada sea NULL si conexion, boca1 y boca2
              son NULL, si no, lo convierte en NULL.
   Eficiencia: O(log R) siendo R la totalidad de los nodos del Switch.
*/
void verificarConmutadorEnRuta(Ruta r, Switch s) {
  RutaIterator ri = iniciarRuta(r);
  Boca bocaAnterior = 0;  // Guarda la boca anterior y el nodo anterior para
  SNode* nodoAnterior;    // acceder al nodo que se debe dejar en NULL
  SNode* currentN = s->root;
  while(!estaAlFinalDeLaRuta(ri)) {  
    nodoAnterior = currentN;
    if (bocaActual(ri) == Boca1) {
      bocaAnterior = Boca1;
      currentN = currentN->boca1;
    } else if (bocaActual(ri) == Boca2) {
      bocaAnterior = Boca2;
      currentN = currentN->boca2;
    }
    AvanzarEnRuta(ri);
  }    
  if(bocaAnterior == 0) {  // Si la ruta era vacía, entonces se analiza el root
    if(s->root->boca1 == NULL && s->root->boca2 == NULL && s->root->conexion == NULL) {
      delete s->root;  // Se libera la posición en memoria para evitar memory leaks
      s->root = NULL;
    }
  } else if(bocaAnterior == Boca1) {
    if(currentN->boca1 == NULL && currentN->boca2 == NULL && currentN->conexion == NULL) {
      delete nodoAnterior->boca1;
      nodoAnterior->boca1 = NULL;
    }
  } else if (bocaAnterior == Boca2) {
    if(currentN->boca1 == NULL && currentN->boca2 == NULL && currentN->conexion == NULL) {
      delete nodoAnterior->boca2;
      nodoAnterior->boca2 = NULL;
    }
  }
  LiberarRutaIterator(ri);
}

// FUNCION AUXILIAR
/* Propósito: Convierte en NULL todos los SNode que tengan conexion, Boca1 y boca2
              en NULL, recorriendo las Rutas dadas en el Switch dado.
   Eficiencia: O(r * log R) siendo r la cantidad de Rutas en rs y R la cantidad total
                            de nodos en el Switch dado.
*/
void verificarConmutadoresEnRutas(Rutas rs, Switch s) {
  RutasIterator ri = iniciarRecorridoDeRutas(rs);
  while(!estaAlFinalDeLasRutas(ri)) {
    verificarConmutadorEnRuta(rutaActual(ri), s); 
    AvanzarASiguienteRuta(ri);
  }
  LiberarRutasIterator(ri);
}

// FUNCION AUXILIAR
/* Propósito: Libera de memoria todas las rutas en rs.
   Eficiencia: O(s) siendo s la longitud de la lista de rutas rs.
*/
void LiberarCadaRutaEn(Rutas rs) {
  RutasIterator ri = iniciarRecorridoDeRutas(rs);
  while(!estaAlFinalDeLasRutas(ri)) {
    LiberarRuta(rutaActual(ri));
    AvanzarASiguienteRuta(ri);
  }
  LiberarRutasIterator(ri);
}

/* Propósito: Desconecta la conexión de la Ruta dada en el Switch dado.
   Precond: La Ruta dada debe existir en el Switch dado.
   Eficiencia: O(l * log R) siendo l la longitud de la ruta r y R la cantidad total
                            de nodos en el Switch dado.
   OBS: 
    * Si en la Ruta dada no hay ninguna conexión, entonces no hace nada.
    * Si se hace una desconexión, entonces se recorre cada conmutador en r para verificar
      que sean consistentes.
*/
void Desconectar(Ruta r, Switch s) {      
  RutaIterator ri = iniciarRuta(r);     // Se crea una lista de rutas y una ruta que posteriormente va a ser modificada, esto
  Rutas rutasAVerificar = emptyRutas(); // para tener rutas que lleven a cada conmutador en la ruta r de manera inversa a r
  Ruta rutaANodo = rutaVacia();         // y poder verificar que sean consistentes, es decir, rutasAVerificar va a guardar en
  SNode* currentN = s->root;            // primera posición a 'r', después a 'r' sacando una boca al final, y asi sucesivamente.
  ConsRuta(rutaANodo, rutasAVerificar);  // Agrega ruta vacía a rutas, ya que se espera recorrer siempre en caso de desconectar
  while(currentN != NULL && !estaAlFinalDeLaRuta(ri)) {
    rutaANodo = copiarRuta(rutaANodo);  // Crea una ruta nueva y la asigna, posteriormente le
    if(bocaActual(ri) == Boca1) {           // agrega una boca dependiendo del camino a tomar según r
      currentN = currentN->boca1;
      SnocBoca(rutaANodo, Boca1);
    } else if (bocaActual(ri) == Boca2) {
      currentN = currentN->boca2;
      SnocBoca(rutaANodo, Boca2);
    }
    ConsRuta(rutaANodo, rutasAVerificar);   // Agrega la ruta anteriormente creada al principio de la lista
    AvanzarEnRuta(ri);
  }
  if(currentN == NULL) { }  // Si la ruta dada no existe entonces no se hace nada.
  else if(currentN->conexion != NULL) { 
    currentN->conexion = NULL;                        // Si la ruta existe y la conexion esta ocupada entonces 
    verificarConmutadoresEnRutas(rutasAVerificar, s); // se recorren las rutas anteriormente guardadas 
  }                                                   // que representan cada boca tomada en sentido inverso
  LiberarCadaRutaEn(rutasAVerificar);  // Se libera de memoria cada ruta creada
  LiberarRutas(rutasAVerificar);                      
  LiberarRutaIterator(ri);                            
} 

/* Propósito: Retorna Rutas disponibles a la distancia dada en el Switch dado.
   Eficiencia: O(R) siendo R todos los nodos del Switch dado.
   OBS: 
    * Se tienen en cuenta las rutas no alcanzables en el Switch.
    * Se recorre el Switch usando BFS de izquierda a derecha.
*/
Rutas disponiblesADistancia(Switch s, int d) {
  Rutas disponiblesADistancia = emptyRutas();
  QNodeSw* current;
  Ruta r1;
  Ruta r2;
  NextsQueueSw aProcesar = emptyQSw();
  EnqueueQSw(aProcesar, rutaVacia(), s->root); 
  while(! isEmptyQSw(aProcesar)) {
    current = DequeueFirstQSw(aProcesar);
    r1 = copiarRuta(current->ruta);  // Se agregan las dos bocas de current
    SnocBoca(r1, Boca1);             // independientemente si existan o sean alcanzables
    r2 = copiarRuta(current->ruta);
    SnocBoca(r2, Boca2);
    if(lenRuta(r1) <= d) {                           
      if(current->node == NULL) {                   
        EnqueueQSw(aProcesar, r1, NULL);  // Si no son alcanzables se agregan las bocas
        EnqueueQSw(aProcesar, r2, NULL);  //  en la queue con nodos NULL.
      } else {
        EnqueueQSw(aProcesar, r1, current->node->boca1);  // Si son alcanzables se agregan a la queue
        EnqueueQSw(aProcesar, r2, current->node->boca2);  // con sus respectivos nodos, sean NULL o no.
      }
    }
    if(lenRuta(current->ruta) == d && (current->node == NULL || current->node->conexion == NULL)) {
      SnocRuta(disponiblesADistancia, current->ruta);  // Si la ruta en current cumple con la distancia
    } else {                                           // pedida se agrega al resultado, si no se libera
      LiberarRuta(current->ruta);                      // de memoria.
    } 
  }
  LiberarQSw(aProcesar);
  return disponiblesADistancia;
}

// FUNCION AUXILIAR
/* Propósito: Libera de memoria todos los nodos inferiores del nodo dado, incluyendo el nodo dado.
   Eficiencia: O(R) siendo R todos los nodos del Switch.
   OBS: Se liberan los nodos utilizando recursión.
*/
void LiberarNodosDeTreeSw(SNode* treeSw) {
  if(treeSw != NULL) {
    LiberarNodosDeTreeSw(treeSw->boca1);
    LiberarNodosDeTreeSw(treeSw->boca2);
    delete treeSw;
  }
}

/* Propósito: Libera el Switch dado de memoria.
   Eficiencia: O(R).
   OBS: Se recorre el Switch utilizando recrusión.
*/
void LiberarSwitch(Switch s) {
  LiberarNodosDeTreeSw(s->root);
  delete s;
}

//------------------------------------------------------------
// FUNCIONES DE SHOW DE SWITCH.
//------------------------------------------------------------
void PadSW(int offset) {
  for(int i=0; i<offset; i++) {
    cout << " ";
  }
}

void MostrarConexion(Ruta r, Cliente c) {
    if (! (c==NULL)) { // La conexión NO está disponible
      cout << " "; ShowRuta(r); cout << " -> " << nombre(c) << endl;
    } else {             // La conexión está disponible
      cout << " "; ShowRuta(r); cout << " -> " << "DISPONIBLE" << endl;
    }
}

void ShowSwitch(Switch s, int offset) {
    // OBS: 
    //   * hace un recorrido BFS del switch, para mostrar las rutas ordenadas
    //   * SOLAMENTE se muestran las rutas ocupadas
    //   * es muy ineficiente en el manejo de rutas
    QNodeSw* current;
    Ruta   r;
    NextsQueueSw aProcesar = emptyQSw();
    EnqueueQSw(aProcesar, rutaVacia(), s->root);
    PadSW(offset); cout << "{" << endl;
    while(! isEmptyQSw(aProcesar)) {
      current = DequeueFirstQSw(aProcesar);
      // Procesar y pasar al siguiente en cada hijo...
      if(! (current->node->boca1 == NULL)) 
        { r = copiarRuta(current->ruta);
          SnocBoca(r, Boca1);
          EnqueueQSw(aProcesar, r, current->node->boca1);
        }
      if(! (current->node->boca2 == NULL)) 
        { r = copiarRuta(current->ruta);
          SnocBoca(r, Boca2);
          EnqueueQSw(aProcesar, r, current->node->boca2);
        }
      PadSW(offset); MostrarConexion(current->ruta,current->node->conexion);
      LiberarRuta(current->ruta);
    }
    PadSW(offset); cout << "}" << endl;
    LiberarQSw(aProcesar);
}
