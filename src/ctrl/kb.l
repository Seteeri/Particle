(def '*pen-v (list 0.0 (- (+ *adv-vert *adv-vert)) 0.0))
(def '*cnt-v 2)


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

            ((= Key 104) # pg-up
             (zoom-cam Sock -3.0))             
            ((= Key 109) # pg-dn
             (zoom-cam Sock 3.0))
  
            (T
             (handle-ascii Key))))))
   

(de handle-ascii (Key)

  # (struct (>> -4 (adr Data)) '((B . 8) (B . 8)))

  (let (Data "ABC"
        Prtcl (new '(+Particle) Data ()))
                
    (draw-str "ABC")

    # Produce verts+edges for rep
    # Push verts into particle
    # Push prtcl into timeline
    # - Timeline is in namespace
        
    (setq *pen-v (list 0.0
                       (- (get *pen-v 2) (+ *adv-vert *adv-vert))
                       0.0))))


(de draw-str (Str)

  (let (Sock-Model (ipc~get-fd> *ipc "MODEL"))
          
    (for C (chop Str)
    
      (let (Off-Vert (get *verts *cnt-v)
            Off      (car Off-Vert)
            Vert     (cdr Off-Vert))

        # Set baseline
        (with Vert
          (=: pos (list (get *pen-v 1) (get *pen-v 2) 0.0)))
                  
        (update-glyph> Vert C)
         
        (setq *pen-v (list (+ (get *pen-v 1) *adv-vert)
                           (get *pen-v 2)
                           0.0))
                                 
        (update-model-matrix> Vert)
        
        (render-vert *ipc Vert Off)
 
        (req-send-obj *ipc
                      Sock-Model
                      Vert
                      SZ-VERT
                      Off)

        (inc '*cnt-v 1)))))

                             
################################################################################


(de handle-ascii-ptr (Key)

  (let (Data (cons Key "ABC")
        Ptrs (struct (>> -4 (adr Data)) '((B . 8) (B . 8))))
        
    (println Ptrs)

    (draw-bytes (get Ptrs 1))
    (draw-bytes (get Ptrs 2))
    
    (setq *pen-v (list 0.0
                       (- (get *pen-v 2) (+ *adv-vert *adv-vert))
                       0.0))))


(de draw-bytes (Bytes)
  # Bytes is list of numbers

  # Create vertex; data = char
  (let (Sock-Model (ipc~get-fd> *ipc "MODEL"))
    
    (for Byte Bytes
    
      (let Str (format Byte)
      
        (for N (chop Str)
        
          (let (Off-Vert (get *verts *cnt-v)
                Off (car Off-Vert)
                Vert (cdr Off-Vert))

            # Reset vertex
            # Pos = baseline = X,0,0
            (with Vert
              (=: pos (list (get *pen-v 1) (get *pen-v 2) 0.0)))
             
            #(with Vert
            #  (println Vert (: pos)))
             
            (update-glyph> Vert N)
             
            (setq *pen-v (list (+ (get *pen-v 1) *adv-vert)
                               (get *pen-v 2)
                               0.0))
                                     
            (update-model-matrix> Vert)
            
            (render-vert *ipc Vert Off)
     
            (req-send-obj *ipc
                          Sock-Model
                          Vert
                          SZ-VERT
                          Off)

            (inc '*cnt-v 1)))
            
          # Inc byte spacing depending on length
          (when T
            (setq *pen-v (list (+ (get *pen-v 1) (* *adv-vert (- 4 (length Str))))
                               (get *pen-v 2)
                               0.0)))
                             
                             T))))


################################################################################


# Handlers
# Do not belong in projview - put in projview in kb subfolder

(de move-cam (Sock Cnt Move)

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


(de zoom-cam (Sock Delta)

   (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
         Sock-Render (ipc~get-fd> *ipc "RENDER"))

     (with *projview

      (inc (:: scale-ortho) Delta)

      (update-mat-proj> *projview)

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