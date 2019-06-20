

#Poss refactor/move to sock? Hmm, not a struct
(de init-socket (Nonblock)
    (let (Sock (socket AF-UNIX
		 (if (= Nonblock 'nonblock)
		     (| SOCK-STREAM SOCK-NONBLOCK)
	             (| SOCK-STREAM))
		 0))
    (when (= Sock -1)
      (quit "(= socket -1)"))
    Sock))


(de init-connection (Path
		     Nonblock)
    
    (let (Sock (init-socket Nonblock)
	  Sockaddr (init-sockaddr 1 Path))
      
      #{
      ;; timeout
      ;; 0 = return immediately
      ;; > 0 = wait indefinitely with delay
      ;;
      ;; Can connect block?
      }#

      (let (Ret (connect-sockfd Sock
		  Sockaddr
		  ADDR-LEN))

	(when (= Ret -1)
	  (quit "(= connect-sockfd -1)"))
	
	(native "@" "free" NIL Sockaddr)
	
	Sock)))


(de send-msg (Sock
	      Ptr
	      Msg)
    #{
    Serialize symbols
    - val  -> get value
    - getl -> get property list
    
    Either pass binary using pr/rd
    Or UTF-8 string through prin/any/str
    
    : (out "x" (pr (list 1 2 3)))
    -> (1 2 3)
    
    : (hd "x")
    00000000  01 04 02 04 04 04 06 03                          ........
    
    : (in "x" (rd))
    -> (1 2 3)
    
    }#
    
    #Use above?
    #Use nanomsg?
    #Use another serialization protocol? MsgPack, JSON (more readily available)
    #Implement pr/rd in Lisp?
    
    (when NIL
      
      #Write string to ptr
      (struct Ptr 'N Msg)
      (println (struct Ptr 'S))
      
      #Send it - copy from ptr to sock
      (send-fd Sock
	       Ptr
	       4096
	       0))

    #Write directly to socket so need not copy to ptr
    (native "@" "write" 'I Sock Msg (+ (length Msg) 0)))


(de send-setup-render ()

    #Send params -> send-memcpy-shm-to-cache
    (send-msg *sock-render
	      *bs-ptr
	      (str (list *params-mmap-nodes
			 *params-mmap-atomic-counter 
			 *params-mmap-draw-indirect  
			 *params-mmap-vertices       
			 *params-mmap-element        
			 *params-mmap-projview       
			 *params-mmap-tex-glyphs
			 '(set-draw T)
			 '(set-serving nil)))))


(de send-clean-up-render ()

    (send-msg *sock-render
	      *bs-ptr
	      "(clean-up-render)"))


(de init-render ()

    #Get from OS environment
    (setq *bs-ptr (native "@" "malloc" 'N 212992))
    #(setq *bs-ptr (init-mmap "bs-ptr" 212992 T))
    #(setq *bs-ptr (open "*bs-ptr"))
    
    (setq *sock-render (init-connection PATH-UDS-RENDER 'block))
    
    (send-setup-render)

    #(shutdown *sock-render)
    #(close-fd *sock-render SHUT-RDWR)
    
    (bye))
    

    
