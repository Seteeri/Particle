#define CELLS (1024*1024/sizeof(cell)) // Heap allocation unit 1MB
// CELLS = 1024*1024 bytes / 16 bytes  = 65536 cells

#define Free(p)         ((p)->car=Avail, Avail=(p))

typedef struct cell {            // PicoLisp primary data type
   struct cell *car;
   struct cell *cdr;
} cell, *any;

typedef struct heap {
   cell cells[CELLS];
   struct heap *next;
} heap;

// This is essentially a linked list of heaps
extern heap *Heaps;
// Pointer to next available cell in heap of heaps
extern cell *Avail;

////////////////////////

/* Allocate cell heap */
void heapAlloc(void) {
   heap *h;
   cell *p;

   // Allocate heap (defaults to 1 MB array of cells)
   h = (heap*)alloc(NULL, sizeof(heap));
   
   // Point next heap to start of Heaps, i.e. itself
   // Point Heaps to recent allocated struct/block of memory
   h->next = Heaps,  Heaps = h;
   
   // If existing:
   // heaps = heap 1
   // [ heap 2 ] -> [ heap 1 ]
   // heaps = heap 2
   
   // Get pointer to last cell
   p = h->cells + CELLS-1;
   
   // Loop backwards to nest cons cells
   // Avail is uninitialized
   // After this func, initSymbols() will use cells and set Avail
   do
      // ... ((point to avail points . ->) . ->)
      p->car=Avail, Avail=p; // #define Free(p)
   while (--p >= h->cells);
}

void resetHeap(void) {
    heap *h;
    
    // Need not call alloc
    // Set Avail ptr...
    h = Heaps
}

// typedef any (*fun)(any);
// #define symPtr(x)       ((any)&(x)->cdr)

// Nil = symPtr(Avail),  Avail = Avail->car->car;  // Allocate 2 cells for NIL
//
// Avail
//   |
// (car . cdr)
//   |
// (car . cdr)
//
//

#define data(c)         ((c).car)
#define Save(c)         ((c).cdr=Env.stack, Env.stack=&(c))
#define drop(c)         (Env.stack=(c).cdr)
#define Push(c,x)       (data(c)=(x), Save(c))
#define Tuck(c1,c2,x)   (data(c1)=(x), (c1).cdr=(c2).cdr, (c2).cdr=&(c1))
#define Pop(c)          (drop(c), data(c))

/* Construct a cell */
any cons(any x, any y) {
   cell *p;

   if (!(p = Avail)) {
      
      cell c1, c2;

      // Push two cons cells onto the stack
      
      c1 = ( x . Env.stack ) ; Env.stack = c1; // Cycle?
      c2 = ( y . Env.stack ) ; Env.stack = c2; // Cycle?
      
      //Push(c1,x);
      //Push(c2,y);
      c1.car = x, c1.cdr = Env.stack, Env.stack = &c1;
      c2.car = y, c2.cdr = Env.stack, Env.stack = &c2;
      
      // GC all cells
      gc(CELLS);
      
      //drop(c1);
      Env.stack = c1.cdr
      
      p = Avail;
   }
   Avail = p->car;
   p->car = x;
   p->cdr = y;
   return p;
}