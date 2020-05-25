(symbols 'wl 'pico)

(def 'SEAT-CAPABILITIES 0)
(def 'SEAT-NAME         1)

# enum wl_seat_capability
(def 'SEAT-CAPABILITY-POINTER  1)
(def 'SEAT-CAPABILITY-KEYBOARD 2)
(def 'SEAT-CAPABILITY-TOUCH    4)


(de display-create ()
  (native "libwayland-server.so" "wl_display_create" 'N))


(de display-add-socket-auto (Display)
  (native "libwayland-server.so" "wl_display_add_socket_auto" 'I Display))


(de display-init-shm (Display)
  (native "libwayland-server.so" "wl_display_init_shm" 'I Display))


(de display-destroy (Display)
  (native "libwayland-server.so" "wl_display_destroy" NIL Display))


(de display-get-event-loop (Display)
  (native "libwayland-server.so" "wl_display_get_event_loop" 'N Display))


(de display-flush-clients (Display)
  (native "libwayland-server.so" "wl_display_flush_clients" NIL Display))




(de event-loop-get-fd (Loop)
  (native "libwayland-server.so" "wl_event_loop_get_fd" 'I Loop))


(de event-loop-dispatch (Loop Timeout)
  (native "libwayland-server.so" "wl_event_loop_dispatch" 'I Loop Timeout))




(de global-create (Display Interface Version Data Bind)
  # Display is opaque
  (native "libwayland-server.so" "wl_global_create" 'N Display (get-ptr> Interface) Version Data Bind))


(de resource-create (Client Interface Version Id)
  (native "libwayland-server.so" "wl_resource_create" 'N Client (get-ptr> Interface) Version Id))


(de resource-set-implementation (Resource Implementation Data Destroy)
  (native "libwayland-server.so" "wl_resource_set_implementation" NIL Resource (get-ptr> Implementation) Data Destroy))


# Inline
(de seat-send-capabilities (Resource Capabilities)
  (native "libwayland-server.so" "wl_resource_post_event" NIL Resource SEAT-CAPABILITIES Capabilities))


#{
  interface-* defined in wayland-private         (prot vers)
  *-interface defined in wayland-server-protocol (fn ptrs)
  Both are generated from XML/scanner
}#

(class +Compositor-Interface)
(dm T (Create-Surface Create-Region)

  (=: ptr (native "@" "malloc" 'N (* 2 8)))
  (struct (: ptr) 'N (cons (lisp 'compositor-create-surface Create-Surface) 8)
                     (cons (lisp 'compositor-create-region Create-Region) 8)))

(dm get-ptr>     ()      (: ptr))


(class +Shell-Interface)
(dm T (Get-Shell-Surface)

  (=: ptr (native "@" "malloc" 'N (* 1 8)))
  (struct (: ptr) 'N (cons (lisp 'shell-get-shell-surface Get-Shell-Surface) 8)))

(dm get-ptr>     ()      (: ptr))


(class +Seat-Interface)
(dm T (Get-Pointer Get-Keyboard Get-Touch Release)

  (=: ptr (native "@" "malloc" 'N (* 4 8)))
  (struct (: ptr) 'N (cons (lisp 'seat-get-pointer Get-Pointer) 8)
                     (cons (lisp 'seat-get-keyboard Get-Keyboard) 8)
                     (cons (lisp 'seat-get-touch Get-Touch) 8)
                     (cons (lisp 'release Release) 8)))

(dm get-ptr>     ()      (: ptr))                     