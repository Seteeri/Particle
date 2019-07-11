(de handle-keyboard (Sock Data)
  (let (Key   (get Data 4)
        State (get Data 5))
    (cond ((and (= Key 106) (= State 1)) # Move camera right
           (move-cam Sock 29 -1.0))
          ((and (= Key 105) (= State 1)) # Move camera left
           (move-cam Sock 29 1.0))
          ((and (= Key 103) (= State 1)) # Move camera up
           (move-cam Sock 30 -1.0))
          ((and (= Key 108) (= State 1)) # Move camera dn
           (move-cam Sock 30 1.0)))))


(de move-cam (Sock Cnt Delta)
   # Pull data from model
   (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
         Sock-Render (ipc~get-fd> *ipc "RENDER")
         Cam         (if *cam *cam (ipc~read-pvm> *ipc Sock-Model)))

      # Modify position
     (let Cam-New (place Cnt Cam (+ (get Cam Cnt) Delta))

       # Push data to model
       #(write-pvm Cam Sock-Model)

       # Push update to render
       #(prinl "SENDING CAM...")
       (ipc~write-pvm> *ipc Cam-New Sock-Render)
       #(prinl "SENT CAM...")
       
       (setq *cam Cam-New))))
