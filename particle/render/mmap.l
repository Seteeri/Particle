(class +Mmap)
(dm T (Path
       FD
       Ptr
       Sz)
  (=: path (when Path @))
  (=: fd   (when FD @))    
  (=: ptr  (when Ptr @))
  (=: size (when Sz @))


(de init-mmap (Path
               Sz
               Create)
    
  (let Fd (memfd-create Path 0)
            
    (when (= Fd -1)
      #Figure out how to get errno...
      (quit "(= memfd-create -1)"))
      
      #Configure the size of the shared memory segment - only on create
    (when Create
      (ftruncate Fd Sz))
      
    #Map shm to address space of process
    (let Ptr (mmap NIL
                   Sz
                   (| PROT-READ PROT-WRITE)
                   (| MAP-SHARED)
                   Fd
                   0)
  
      (when (=0 Ptr) #null ptr
        (quit "(= mmap -1)"))

      # zero memory?
      (new '(+Mmap) Path Fd Ptr Sz))))


#{
(de init-mmap-ipc (P)
    (init-mmap (get P 2)
        (get P 3))
        T))
}#

(de init-mmap-nodes ()
  (init-mmap (get *params-mmap-nodes 2)
             (get *params-mmap-nodes 3)
             T))


(de init-mmap-atomic-counter ()
  (init-mmap (get *params-mmap-atomic 2)
             (get *params-mmap-counter 3)
             T))


(de init-mmap-draw-indirect ()
  (let (Mmap (init-mmap (get *params-mmap-draw-indirect 2)
                        (get *params-mmap-draw-indirect 3)
              T)
        Ptr (get Mmap 'ptr))
          
    #(println "FD" (get Mmap 'fd))

    #Use struct function to write to pointer     
    (struct Ptr 'N
            (6 . 4)
            (cons *inst-max 4)
            (0 . 4)
            (0 . 4)
            (0 . 4))

    #(println "Read 6 int32" (struct Ptr '(I . 6)))
    
    Mmap))


(de init-mmap-vertices ()
  #{
  ;;top right, bottom right, bottom left, top left
  ;;
  ;;3---0
  ;;| / |
  ;;2---1
  ;;
  ;;ccw: 0 2 1 0 3 2
  }#
  (let (Mmap (init-mmap (get *params-mmap-vertices 2)
                        (get *params-mmap-vertices 3)
              T)
        Ptr (get Mmap 'ptr))
    (struct Ptr 'N (list -1.0  1.0  1.0  0.0  1.0
                          1.0  0.0  0.0  1.0
                          0.0  0.0  0.0  1.0
                          0.0  1.0  0.0  1.0))
    Mmap))


(de init-mmap-element ()
  (let (Mmap (init-mmap (get *params-mmap-element 2)
                        (get *params-mmap-element 3)
                        T)
        Ptr (get Mmap 'ptr))
    (struct Ptr 'N
            (0 . 4)
            (2 . 4)
            (1 . 4)
            (0 . 4)
            (3 . 4)
            (2 . 4))
    Mmap))


(de init-mmap-projview ()
  (let (Mmap (init-mmap (get *params-mmap-projview 2)
                        (get *params-mmap-projview 3)
                        T)
        Ptr (get Mmap 'ptr))
    (wr-mat-proj-ptr> *projview (get Mmap 'ptr))
    (wr-mat-view-ptr> *projview (+ (get Mmap 'ptr) 16))
    Mmap))
