#{
  Pointer
  
  - Placement is relative to ref
  - Selection is ref
    - So to do below, must select first in list
  - Mod + WASD
    - Left:ASCII + Right:R-Ctrl

  * New - ?
  
  * Select - R-Alt+WASD
    * L/R - move list items
    * U/D - move lists

      (*1 2 (3 4) 5 6)

      -> R = 2
      -> R = 5
      -> R = 6
      -> ...

      -> R = 2
      -> D = 3
      -> R = 4
      -> R = no move

      -> R = 2
      -> D = 3
      -> R = 4
      -> U = 5
      -> R = 6
      -> R = no move
         * User can move up to timeline list and create new list

        (1 2 (3 4) 5 6 *)
                     

        (1 2 (3 4) 5 6) *
                        

        (1 2 (3 4) 5 6) (*)
                        

        * Press up anytime while on 3 or 4 to move to prev list in list or prev item (2) in parent list
        * Press dn anytime while on 3 or 4 to move to next list in list or next item (5) in parent list


    * Shift + Alt + L/R ? - move to start/end item in list
    * Shift + Alt + U/D ? - move to top-level list

    Slurp+Dir / Barf+Dir:
    * Ctrl + Shift + Alt + L/R - push prev/next item in list
    * Ctrl + Shift + Alt + U/D - append/con prev/next list

  * Hold both R-Alt + R-Ctrl to move together
    - Or by itself to move cursor to pointer

  * Place

    * Selection is always before cursor (old->new)
    * Push unilaterally to match data structure
    * Use toggle switch? TAB? Enter?

    1: Given
    
    (let (a b
          c d)

    -> Produce (Insert after b, before c)

    (let (a b
          e f
          c d)

    Enter list, get to b
    (let (a @ b
          c d)
    
    Press +Dn to move below (below start prev row)
    (let (a b
          @
          c d)

    Press +Up to return to above (end of prev row)
    (let (a b @
          c d)

    (let (a b
          e f @
          c d)

    * Can press +Dn to produce another row or move sel to d to continue
    push

    * So what does Dn/Up do in middle???
      -> Currently, nothing or same as Lt/Rt
    

    Issue

    (let (a b
          c d
          @)

    * How does it know how far to move vertically?
      -> Store lines in list particle - store portion of list/CDR
  
}#


############################
# Placement (Semantic Space)

(de cmd-place-up (Keysym)
  (println "Control-R + W: Place Up")
  (setq *place-cur 'above))
  
(de cmd-place-dn (Keysym)
  (println "Control-R + S: Place Dn")
  (setq *place-cur 'under))


(de cmd-place-le (Keysym)
  (println "Control-R + A: Place Le")
  (setq *place-cur 'before))
  
(de cmd-place-ri (Keysym)
  (println "Control-R + D: Place Ri")
  (setq *place-cur 'after))


#########################
# Selection (User Space?)

(de cmd-select-up (Keysym)
  (println "Control-R + W: Select Up")  
  (when *lists-ptr
    (pop '*lists-ptr)))

  
(de cmd-select-dn (Keysym)
  (println "Control-R + S: Select Dn")

  # Push cur list into past list?

  # Enter list
  # Search for subsequent list in current list

  (let (Part (car-ptr)
        Last (get Part 'data))

    (when (lst? Last)

      (println "BEFORE")
      (pretty *lists-ptr)
      (prinl)

      (push '*lists-ptr Part)

      (println "AFTER")
      (pretty *lists-ptr)
      (prinl)

      # Move pointer or change color
      #(change-col-particle Part *col-magenta)

      T)))


# NEXT
# (f d s a)
#  * ->

# PREV
# (f d s a)
#     <- *

(de cmd-select-le (Keysym)
  (println "Control-R + A: Select Le")

  # Have to get location in current list to get previous...not efficient
  # or...make particle doubly-linked list
  # or...r-tree search

  # Cache this list

  # Traverse list until current particle found
  # While iterating save, prev

  (let (Prev NIL
        Part (car-ptr))

    (until (or (= Part *part-ptr)
               (= Part 'PARTICLE)
               (not Part))
      (setq Prev Part)
      (setq Part (get Part 'next)))

    (setq *part-ptr Prev)
    (adv-ptr 4.0)))

  
(de cmd-select-ri (Keysym)
  (println "Control-R + D: Select Ri")

  # Move to next item
  # Check current particle next value

  # Save prev for efficiency

  (unless (= *part-ptr 'PARTICLE)
    (with *part-ptr
      (println "*part-ptr:next" (: next))
      (if (= (: next) 'PARTICLE)
        (prog
          (println "1" "NEXT=" 'PARTICLE "CUR=" (: data))
          #(setq *atom-pre-ptr *part-ptr)
          (setq *part-ptr 'PARTICLE))
        (prog
          (println "2" "NEXT=" (get (: next) 'data) "CUR=" (: data))
          #(setq *atom-pre-ptr *part-ptr)
          (setq *part-ptr (: next))
          (adv-ptr -4.0))))))


#################
# Euclidean Space
# or grid move

(de cmd-translate-ptr (Cnt Move)

  (with *vert-ptr
      (=: pos (list (+ (get (: pos) 1) (get Move 1))
                    (+ (get (: pos) 2) (get Move 2))
                    (get (: pos) 3))))

  (update-model-matrix> *vert-ptr)

  (cmd-update-vert *ipc *vert-ptr))


(de cmd-translate-ptr-px (Keysym) (cmd-translate-ptr 1 (list (*/ *adv-vert 2.0 1.0)   0.0)))
(de cmd-translate-ptr-nx (Keysym) (cmd-translate-ptr 1 (list (*/ *adv-vert -2.0 1.0)  0.0)))
(de cmd-translate-ptr-py (Keysym) (cmd-translate-ptr 2 (list 0.0 (*/ (+ *adv-vert *adv-vert) 2.0 1.0))))
(de cmd-translate-ptr-ny (Keysym) (cmd-translate-ptr 2 (list 0.0 (*/ (+ *adv-vert *adv-vert) -2.0 1.0))))