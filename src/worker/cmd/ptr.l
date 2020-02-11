######################
# Semantic (Selection)

(de cmd-assign-ptr-pre (Keysym)
  # Move to next item in the list
  T)


(de cmd-assign-ptr-nxt (Keysym)
  # Move to prev item in the list
  T)


(de cmd-assign-ptr-out (Keysym)
  # Move to outer list
  T)


(de cmd-assign-ptr-in (Keysym)
  # Move to inner list
  T)


###########
# Euclidean


(de cmd-trans-ptr (Cnt Mov)
  # Move p0
  (let (Ptr (car *part-ptrs)
        Pos (get-origin> Ptr))
    (mov> Ptr
          (place Cnt
                 Pos
                 (+ (get Pos Cnt) Mov)))))


(de cmd-trans-ptr-px (Keysym)
  (cmd-trans-ptr 1 (*/ *adv-vert 1.0 1.0)))
  
(de cmd-trans-ptr-nx (Keysym)
  (cmd-trans-ptr 1 (*/ *adv-vert -1.0 1.0)))
  
(de cmd-trans-ptr-py (Keysym)
  (cmd-trans-ptr 2 (*/ (+ *adv-vert *adv-vert) 1.0 1.0)))
  
(de cmd-trans-ptr-ny (Keysym)
  (cmd-trans-ptr 2 (*/ (+ *adv-vert *adv-vert) -1.0 1.0)))


###########
# Manhatten
