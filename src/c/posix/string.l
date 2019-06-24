#string.h

(de memcpy (Dest Src N)
    (native "@" "memcpy" NIL Dest Src N))

(de memmove (Dest Src N)
    (native "@" "memmove" NIL Dest Src N))
