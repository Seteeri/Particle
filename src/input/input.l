#{
    Global variables start with an asterisk "*"
    Global constants may be written all-uppercase
    Functions and other global symbols start with a lower case letter
    Locally bound symbols start with an upper case letter
    Local functions start with an underscore "_"
    Classes start with a plus-sign "+", where the first letter
        is in lower case for abstract classes
        and in upper case for normal classes
    Methods end with a right arrow ">"
    Class variables may be indicated by an upper case letter
}#


(symbols 'input 'pico)


# C library paths

# Paths
(def 'PATH-UDS-MODEL  "/tmp/protoform-model.socket")
(def 'PATH-UDS-INPUT  "/tmp/protoform-input.socket")
(def 'PATH-UDS-RENDER "/tmp/protoform-render.socket")


# Set math precision to 6 decimals
(scl 6)


# Wrapper libraries
(load "/home/user/Git/particle/src/c/epoll.l")
(load "/home/user/Git/particle/src/c/li.l")
(load "/home/user/Git/particle/src/c/socket.l")
# Lisp libraries
(load "/home/user/Git/particle/src/misc.l")
(load "/home/user/Git/particle/src/ipc/ipc.l")
(load "/home/user/Git/particle/src/prot/prot.l")


#Globals
(def '*input)
(def '*ipc)


(de get-rmem-default ()
  (pipe
   (call "cat" "/proc/sys/net/core/rmem_default")
   (read)))


(de init-input ()

  (let (Argv (argv))

    (setq *width  (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "width invalid")))
          *height (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "height invalid")))))

  (setq *ipc (new '(ipc~+IPC)
                  PATH-UDS-INPUT 'ipc~block handle-client accept-client))

  (setq *input (new '(li~+Li)))
  (ipc~add-conn> *ipc (get *input 'li~fd-context) "LI" handle-fd-li))


(de run-input (Timeout)

  (prinl (usec) " | run-input | Starting epoll...")
  (loop (ipc~poll> *ipc Timeout)))


(de handle-client (Sock)
  (let (Msg (recv-msg *ipc Sock)
        Data (any Msg))
    (when (= Data "bye")
      (send-msg-conns *ipc "bye")
      (ipc~disconnect-sock Sock)
      (end)
      (println "Input says bye!")
      (bye))))


(de handle-fd-li ()

  (with *input

    (li~dispatch (: li~context))

    (loop

       (setq Event (li~get-event (: li~context)))
       (T (= Event 0))

       (send-event Event)

       (li~event-destroy Event)
       (li~dispatch (: li~context)))))


(de send-event (Event)

  (let (Type (li~event-get-type Event)
        Name-Type (li~get-event-type-name Type))

    #(prinl (usec) " | handle-context   | Event: " Type "=" Name-Type)

    (let Msg (pack "("
                   (glue " " (make (link Name-Type)
                                   (link (build-ev-lst-str Event Type))))
                   ")")

      #(when (= Type li~KEYBOARD-KEY)
      #  (println Msg))

      # Workers will send messages to input when they are ready
      (send-msg-clients *ipc Msg))))


(de build-ev-lst-str (Event Type)

    (cond ((= Type li~KEYBOARD-KEY)
           (glue " " (list (format (li~kb-get-time Event))
                           (format (li~kb-get-time-usec Event))
                           (format (li~kb-get-key Event))
                           (format (li~kb-get-key-state Event))
                           (format (li~kb-get-seat-key-count Event)))))

          ((= Type li~POINTER-MOTION)
           (glue " " (list (format (li~ptr-get-time Event))
                           (format (li~ptr-get-time-usec Event))
                           (format (li~ptr-get-dx Event) *Scl)
                           (format (li~ptr-get-dy Event) *Scl)
                           (format (li~ptr-get-dx-unaccelerated Event) *Scl)
                           (format (li~ptr-get-dy-unaccelerated Event) *Scl))))

          ((= Type li~POINTER-MOTION-ABSOLUTE)
           (glue " " (list (format (li~ptr-get-time Event))
                           (format (li~ptr-get-time-usec Event))
                           (format (li~ptr-get-absolute-x Event) *Scl)
                           (format (li~ptr-get-absolute-y Event) *Scl)
                           (format (li~ptr-get-absolute-x-transformed Event *width) *Scl)
                           (format (li~ptr-get-absolute-y-transformed Event *height) *Scl))))

          ((= Type li~POINTER-BUTTON)
           (glue " " (list (format (li~ptr-get-time Event))
                           (format (li~ptr-get-time-usec Event))
                           (format (li~ptr-get-button Event))
                           (format (li~ptr-get-button-state Event))
                           (format (li~ptr-get-seat-button-count Event)))))

          ((= Type li~POINTER-AXIS)
           (glue " " (list (format (li~ptr-get-time Event))
                           (format (li~ptr-get-time-usec Event))
                           (format (li~ptr-has-axis Event Axis))
                           (li~get-pointer-axis-source-name (li~ptr-get-axis-source Event))
                           (when (= (li~ptr-has-axis Event Axis) 1)
                             (format (li~ptr-get-axis-value Event li~POINTER-AXIS-SCROLL-HORIZONTAL) *Scl)
                             (format (li~ptr-get-axis-value Event li~POINTER-AXIS-SCROLL-VERTICAL)   *Scl)
                             (format (li~ptr-get-axis-value-discrete Event li~POINTER-AXIS-SCROLL-HORIZONTAL) *Scl)
                             (format (li~ptr-get-axis-value-discrete Event li~POINTER-AXIS-SCROLL-VERTICAL)   *Scl)))))

          ((or (= Type li~TOUCH-UP)
               (= Type li~TOUCH-CANCEL)
               (= Type li~TOUCH-FRAME))
           (glue " " (list (format (li~tch-get-time Event))
                           (format (li~tch-get-time-usec Event)))))

          ((or (= Type li~TOUCH-DOWN)
               (= Type li~TOUCH-MOTION))
           (glue " " (list (format (li~tch-get-time Event))
                           (format (li~tch-get-time-usec Event))
                           (format (li~tch-get-x Event) *Scl)
                           (format (li~tch-get-y Event) *Scl)
                           (format (li~tch-get-x-transformed Event *width) *Scl)
                           (format (li~tch-get-y-transformed Event *height) *Scl))))))

(de end ()
  (ipc~end> *ipc)
  (li~end> *input))


(de main ()

    #(push '*Bye '(end-input))
    (init-input)
    (run-input -1)
    (bye))
