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
  T)


(de cmd-assign-ptr-in (Keysym)
  # Move to inner list
  T)


###########
# Euclidean


(de cmd-translate-ptr (Cnt Mov)
  (let ((X Y Z) *pos-draw
        (Xd Yd Zd) Mov)
    (setq *pos-draw (list (+ X Xd)
                          (+ Y Yd)
                          Z))))


(de cmd-translate-ptr-px (Keysym) (cmd-translate-ptr 1 (list (*/ *adv-vert 1.0 1.0)   0.0)))
(de cmd-translate-ptr-nx (Keysym) (cmd-translate-ptr 1 (list (*/ *adv-vert -1.0 1.0)  0.0)))
(de cmd-translate-ptr-py (Keysym) (cmd-translate-ptr 2 (list 0.0 (*/ (+ *adv-vert *adv-vert) 1.0 1.0))))
(de cmd-translate-ptr-ny (Keysym) (cmd-translate-ptr 2 (list 0.0 (*/ (+ *adv-vert *adv-vert) -1.0 1.0))))


###########
# Manhatten
