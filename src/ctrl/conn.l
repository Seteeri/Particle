(de handle-keyboard (Sock Data)
  (let (Key   (get Data 4)
        State (get Data 5))
    (cond ((and (= Key 106) (= State 1)) # Move camera right
           (move-cam Sock 1 '(1.0 0.0)))
          ((and (= Key 105) (= State 1)) # Move camera left
           (move-cam Sock 1 '(-1.0 0.0)))
          ((and (= Key 103) (= State 1)) # Move camera up
           (move-cam Sock 2 '(0.0 1.0)))
          ((and (= Key 108) (= State 1)) # Move camera dn
           (move-cam Sock 2 '(0.0 -1.0))))))


(de move-cam (Sock Cnt Move)
   # Pull data from model
   (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
         Sock-Render (ipc~get-fd> *ipc "RENDER"))

     (with *projview

      (=: position (list (+ (get (: position) 1) (get Move 1))
                         (+ (get (: position) 2) (get Move 2))
                         10.0))

      (update-mat-view> *projview)     
      
      (req-send-obj *ipc
                    Sock-Render
                    *projview
                    128
                    0)
                    
      # Push to model also
      (req-send-obj *ipc
                    Sock-Model
                    *projview
                    128
                    0))))


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
                208
                Off))


(de update-pvm (Ipc Projview)
  (req-send-obj Ipc
                (ipc~get-fd> Ipc "RENDER")
                Projview
                128
                0))