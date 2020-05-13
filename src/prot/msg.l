(de send-msgs (Ipc Sock Msgs)

  #{
    Msgs are a list
    
    ( (Sexpr1 Bin1)
      (Sexpr2 Bin2)
      (Sexpr3 Bin3)
      ...)

    However, they are written back to back (and the list not directly written)

    Try to format all into the pointer, and then return unsent msgs or do
    send all
  }#

  (let Len-Sock 0

    # Ensure it will fit
    # Conservative estimate could be half
  
    (for Msg Msgs
      (let ((Sexpr Bin Sz-Bin) Msg)
        (inc 'Len-Sock (wr-msg Ipc Sexpr Bin Sz-Bin))))

    (let Len-Sock (wr-msg Ipc Sexpr Bin Sz-Bin)
      
      (ipc~send-sock> Ipc Sock T Len-Sock))))


(de wr-msg (Ipc Sexpr Bin Sz-Bin)

  #{
  
    Msg Struct:
    * Consist of lisp s-exp and optional bin data
    
    Head:
    Len-Msg
    Len-Sexp
    Sz-Bin
    
    Body:
    Str-Sexpr + null byte
    Bin
    
    Poss simplify to:
    Len-Sexp
    Sexp
    Len-Bin
    Bin
  
  }#

  (let (Bin    (if Bin @ 0)
        Sz-Bin (if Sz-Bin @ 0)
        Len-Sexpr (+ (length Sexpr) 1)
        Len-Body  (+ Len-Sexpr Sz-Bin)
        Len-Msg   (+ 4 4 Len-Body)
        Len-Sock  (+ 4 4 4 Len-Body))

    (with Ipc

      (struct (: ipc~buf-wr) 'N
              (cons Len-Msg 4)
              (cons Len-Sexpr 4)
              (cons Sz-Bin 4)
              (cons Sexpr Len-Sexpr) 0)

      (when Bin
        (native "@" "memcpy" NIL
                (+ (: ipc~buf-wr) 4 4 4 Len-Sexpr) Bin Sz-Bin)))

    Len-Sock))


# SEND-MSG
# Send single msg


(de send-msg (Ipc Sock Sexpr Bin Sz-Bin)

  # Could do this unblocked, if delayed, then server is lagging and could warn

  (let Len-Sock (wr-msg Ipc Sexpr Bin Sz-Bin)
    (ipc~send-sock> Ipc Sock T Len-Sock)))


(de send-msg-clients (Ipc Msg)
  (for Cli (get Ipc 'ipc~clients)
    #(prinl-info "send-msg-clients" (str Cli))
    # Check for error
    (send-msg Ipc (car Cli) Msg)))


(de send-msg-conns (Ipc Msg)
  (for Conn (get Ipc 'ipc~conns)
    # Check for error
    (send-msg Ipc (car Conn) Msg)))


# RECV
# Recv single msg - wait on all data - pass flag?

(de recv-msg (Ipc Sock)

  (let ((Len-Msg Len-Sexpr Sz-Bin) (recv-msg-len Ipc Sock))

    #(prinl-info "recv-msg" (pack "RECV " Len-Msg " " Len-Sexpr " " Sz-Bin))

    (when (and Len-Msg
               (ipc~recv-sock> Ipc Sock T (+ Len-Sexpr Sz-Bin) sock~MSG-WAITALL))

      (with Ipc
        (pack (struct (: ipc~buf-rd) (cons 'C Len-Sexpr)))))))


(de recv-msg-with-bin (Ipc Sock)

  (let ((Len-Msg Len-Sexpr Sz-Bin) (recv-msg-len Ipc Sock))

    #(prinl-info "recv-msg" (pack "RECV " Len-Msg " " Len-Sexpr " " Sz-Bin))

    (when (and Len-Msg
               (ipc~recv-sock> Ipc Sock T (+ Len-Sexpr Sz-Bin) sock~MSG-WAITALL))

      (with Ipc
        (cons (pack (struct (: ipc~buf-rd) (cons 'C Len-Sexpr)))
              Sz-Bin)))))


(de recv-msg-len (Ipc Sock)
  (when (ipc~recv-sock> Ipc Sock T 12 sock~MSG-WAITALL)
    (struct (get Ipc 'ipc~buf-rd) '(I . 3))))
