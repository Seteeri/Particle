# Should these simply be methods of pointer?


###########
# Selection

(de cmd-assign-ptr-pre (Keysym)
  # Move to next item in the list
  T)


(de cmd-assign-ptr-nxt (Keysym)
  # Move to prev item in the list
  T)


(de cmd-assign-ptr-out (Keysym)
  # Move to outer list
  
  #{
  # If at root, do nothing
  (unless (= *part-ptr *root-ptr)

    # Set *lst-ptr from popped *lsts-ptr

    T)
  }#
  
  T)


(de cmd-assign-ptr-in (Keysym)
  # Move to inner list

  # *part-ptr  -> *root-ptr
  # *lst-ptr   -> *lst-ptr
  # *lists-ptr -> *lsts-ptr (track history of lists)
  
  # Push *lst-ptr to *lsts-ptr
  # Set *lst-ptr from pointer

  # Make sure current ptr is a list
  # CAR and CDR should both be lists
  
  # Normally get from ptr/cur pos but for now use end of list
  # since we have no implemented selecting yet
  # Selecting is a pointer to a cons cell
  # Question is how to mark it - for now, use color - yellow
  
  # If you push/cons unto a symbol, will get improper list
  T)


###########
# Euclidean


(de cmd-translate-ptr (Cnt Move)
  (with *vert-ptr
      (=: pos (list (+ (get (: pos) 1) (get Move 1))
                    (+ (get (: pos) 2) (get Move 2))
                    (get (: pos) 3))))
  (update-model-matrix> *vert-ptr)
  (cmd-update-vert *ipc *vert-ptr))


(de cmd-translate-ptr-px (Keysym) (cmd-translate-ptr 1 (list (*/ *adv-vert 1.0 1.0)   0.0)))
(de cmd-translate-ptr-nx (Keysym) (cmd-translate-ptr 1 (list (*/ *adv-vert -1.0 1.0)  0.0)))
(de cmd-translate-ptr-py (Keysym) (cmd-translate-ptr 2 (list 0.0 (*/ (+ *adv-vert *adv-vert) 1.0 1.0))))
(de cmd-translate-ptr-ny (Keysym) (cmd-translate-ptr 2 (list 0.0 (*/ (+ *adv-vert *adv-vert) -1.0 1.0))))


###########
# Manhatten
