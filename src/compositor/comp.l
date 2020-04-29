#pil src/compositor/comp.l -comp~main +
(symbols 'comp 'pico)


# Set math precision to 6 decimals
# (scl 6)


(def 'NAME-PID "COMP")


# Wrapper libraries
(load "/home/user/Particle/particle/src/c/mathc.l")
(load "/home/user/Particle/particle/src/c/posix/mman.l")
(load "/home/user/Particle/particle/src/c/posix/stdio.l")
(load "/home/user/Particle/particle/src/c/posix/string.l")
(load "/home/user/Particle/particle/src/c/posix/unistd.l")
(load "/home/user/Particle/particle/src/c/epoll.l")
(load "/home/user/Particle/particle/src/c/socket.l")
(load "/home/user/Particle/particle/src/c/wayland/util.l")
(load "/home/user/Particle/particle/src/c/wayland/private.l")
(load "/home/user/Particle/particle/src/c/wayland/server-protocol.l")
# Lisp libraries
(load "/home/user/Particle/particle/src/misc.l")
(load "/home/user/Particle/particle/src/ipc/ipc.l")
(load "/home/user/Particle/particle/src/prot/conn.l")
(load "/home/user/Particle/particle/src/prot/msg.l")
(load "/home/user/Particle/particle/src/prot/obj.l")


(def '*display)
(def '*clients) 
(def '*surfaces)

#{
  interface-* defined in wayland-private         (prot vers)
  *-interface defined in wayland-server-protocol (fn ptrs)
  Both are generated from XML/scanner
}#
(def '*compositor-interface)
(def '*shell-interface)
(def '*seat-interface)

#{
(def '*xdg-wm-base-interface (native "@" "malloc" 'N (* 4 8)))
(def '*interface-xdg-wm-base (new '(+Interface)))
(struct *xdg-wm-base-interface 'N (cons (lisp 'xdg-wm-base-destroy xdg-wm-base-destroy) 8)
                                  (cons (lisp 'xdg-wm-base-create-positioner xdg-wm-base-create-positioner) 8)
                                  (cons (lisp 'xdg-wm-base-get-xdg-surface xdg-wm-base-get-xdg-surface) 8)
                                  (cons (lisp 'xdg-wm-base-pong xdg-wm-base-pong) 8))
}#


(de init-comp ()

  # https://github.com/eyelash/tutorials/tree/master/wayland-compositor

  (setq *compositor-interface (new '(wl~+Compositor-Interface) compositor-create-surface compositor-create-region))
  (setq *shell-interface      (new '(wl~+Shell-Interface) shell-get-shell-surface))
  (setq *seat-interface       (new '(wl~+Seat-Interface) seat-get-pointer seat-get-keyboard seat-get-touch seat-release))

  (setq *display (wl~display-create))
  (wl~display-add-socket-auto *display)

	(wl~global-create *display wl~*interface-compositor  3 0 (lisp 'compositor-bind  compositor-bind))
	(wl~global-create *display wl~*interface-shell       1 0 (lisp 'shell-bind       shell-bind))
	#(wl~global-create *display wl~*interface-xdg-wm-base 1 0 (lisp 'xdg-wm-base-bind xdg-wm-base-bind))
	(wl~global-create *display wl~*interface-seat        1 0 (lisp 'seat-bind        seat-bind))

	(wl~display-init-shm *display))


(de run-comp (Timeout)

  (let (Event-Loop (wl~display-get-event-loop *display)
        Fd-Wl      (wl~event-loop-get-fd Event-Loop))

    # Add fd to epoll
    # On IN, call loop-dispatch

    (println "Running loop...")

    (loop

      #(println "DISPATCHING...")
      (wl~event-loop-dispatch Event-Loop 0)
      #(println "DISPATCHING...DONE")
      
      # Poll input
      # Send events

      #(println "FLUSHING...")
      (wl~display-flush-clients *display)
      #(println "FLUSHING...DONE")

      # Draw
      (wait 1)

      T))

	(wl~display-destroy *display))      


(de compositor-bind (Client Data Version Id)
  (println "compositor-bind" Client Data Version Id)
	(let Res (wl~resource-create Client wl~*interface-compositor 1 Id)
    (wl~resource-set-implementation Res *compositor-interface 0 0)))


(de shell-bind (Client Data Version Id)
  (println "shell-bind" Client Data Version Id)
	(let Res (wl~resource-create Client wl~*interface-shell 1 Id)
    (wl~resource-set-implementation Res *shell-interface 0 0))
  (println "DONE"))


(de seat-bind (Client Data Version Id)
  (println "seat-bind" Client Data Version Id)
	(let Res (wl~resource-create Client wl~*interface-seat 1 Id)
    (wl~resource-set-implementation Res *seat-interface 0 0)
    (wl~seat-send-capabilities Res (| wl~SEAT-CAPABILITY-POINTER wl~SEAT-CAPABILITY-KEYBOARD))))


(de compositor-create-surface ())
(de compositor-create-region  ())
(de shell-get-shell-surface ())
(de seat-get-pointer ())
(de seat-get-keyboard ())
(de seat-get-touch ())
(de seat-release ())


(de xdg-wm-base-bind (Client Data Version Id)
  (println "xdg-bind"))


(de main ()

  (init-comp)
  (run-comp -1)
  (println 'Exiting...)
  (bye))
