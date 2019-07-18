(de handle-keyboard (Sock Data)
  (let (Key   (get Data 4)
        State (get Data 5))
    (when (= State 1)
      (cond ((= Key 106) 
             # Move camera right
             (move-cam Sock 1 '(1.0 0.0)))
            ((= Key 105) 
             # Move camera left
             (move-cam Sock 1 '(-1.0 0.0)))
            ((= Key 103) 
             # Move camera up
             (move-cam Sock 2 '(0.0 1.0)))
            ((= Key 108) 
             # Move camera dn
             (move-cam Sock 2 '(0.0 -1.0)))
            
            #(= Key 30) # a
            (T
             (handle-a Key))))))


(de handle-a (Key)
  #{
  * Create cons cell
  * Push cons cell into timeline list
  * To draw cons cell:
    * Get CAR/CDR encoded pointers
    * Load vertices/edges
  * Draw all
  }#
      
  # Need access to metrics
  # Either load in each worker or pull from model each time
      
  # Create vertex; data = char
  (let Data "a"
  
    # Get CAR/CDR ptrs/bytes
    (println (struct (>> -4 (adr Data)) '((B . 8) (B . 8))))

    # Load vertices
    
    # Decode CAR/CDR -> Num, Sym, Cons
    # For now, will be T-Sym
    # Pressing "a" multiple times results in:
    # (cons PL "a") (cons PL "a") # cons is really Vertex
    # Both cons CDR point to same symbol
      
    #{
    

              'a'
              |  
              |  
       12345678<-87654321
                 |      
       +---------+      
       |                
  [PL][.]               
    |                   
    +... (list of GL data for each particle inc edges)
    
    (* vert (+ 48n 2e))
    # data for the val - modify to modify val
    # data for the data - cannot be visualized? use functions to modify
    
    
    }#
  
    (let Cnt 1
      (for N (chop (format (adr Data)))
        (let (Off-Vert (get-vert-off Cnt)
              Vert (req-recv-obj *ipc 
                                 (ipc~get-fd> *ipc "MODEL")
                                 (new '(+Vertex))
                                 SZ-VERT
                                 Off-Vert))
          (update-glyph> Vert N)
          (with Vert
            (=: pos (list (* Cnt *adv-glyph)
                          0.0
                          0.0)))
          (update-model-matrix> Vert)
          (update-vert *ipc Vert Off-Vert)
          (inc 'Cnt))))
    
    T)
    
  T)
  

(de move-cam (Sock Cnt Move)
   # Pull data from model
   (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
         Sock-Render (ipc~get-fd> *ipc "RENDER"))

     (with *projview

      (=: pos (list (+ (get (: pos) 1) (get Move 1))
                    (+ (get (: pos) 2) (get Move 2))
                    10.0))

      (update-mat-view> *projview)     
      
      # Either send relevant data or send entire data
      # -> Minimize render processing time
      
      # Pass list of socks so need only serialize once
      
      (req-send-obj *ipc
                    Sock-Render
                    *projview
                    SZ-PV
                    0)
                    
      # Push to model also
      (req-send-obj *ipc
                    Sock-Model
                    *projview
                    SZ-PV
                    0))))


################################################################################


(de req-send-obj (Ipc Sock Obj Sz-Obj Off-Obj)
  # Better to write all strings/bytes to buf-wr, then send in one go

  (send-msg Ipc
            (fmt-msg-send> Obj Off-Obj)
            Sock)

  (send-obj Ipc Sock Obj Sz-Obj Off-Obj))
  

(de req-recv-obj (Ipc Sock Obj Sz-Obj Off-Obj)
  # Either pass in object or instance new one

  (send-msg Ipc
            (fmt-msg-recv> Obj Off-Obj)
            Sock)

  (recv-obj Ipc Sock Obj Sz-Obj Off-Obj))


(de update-vert (Ipc Vert Off)
  (req-send-obj Ipc
                (ipc~get-fd> Ipc "RENDER") # Pass string and auto look up?
                Vert
                SZ-VERT
                Off))


(de update-pvm (Ipc Projview)
  (req-send-obj Ipc
                (ipc~get-fd> Ipc "RENDER")
                Projview
                SZ-PV
                0))