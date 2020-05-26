//clang -fPIC -shared -O3 -o libh64s.so hash64shift.c

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

extern uint64_t
h64s(uint64_t x)
{
  x = (~x) + (x << 21);
  x = x ^ (x >> 24);
  x = (x + (x << 3)) + (x << 8);
  x = x ^ (x >> 14);
  x = (x + (x << 2)) + (x << 4);
  x = x ^ (x >> 28);
  x = x + (x << 31);
  
  //printf("%" PRId64 "\n", x);
  return x;
}

/*

  (do (** 2 19)
  (let (C (cons NIL (box))
  A (adr C)
  H (native PATH-H64S "h64s" 'N A))
  (set C H)
  (idx '*points C T)))

  #(pretty *points)
  (prinl) (prinl) (println (** 2 19) " : " (depth *points)) (prinl) 
*/
