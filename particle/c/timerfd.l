(symbols 'timerfd 'pico)

(local) (create fd)

(def 'CLOCK-REALTIME 0)
(def 'CLOCK-MONOTONIC 1)

(def '*itimerspec-new (native "@" "malloc" 'N (* 4 8)))
(def '*itimerspec-old (native "@" "malloc" 'N (* 4 8)))


(de create (Clockid Flags)
  (native "@" "timerfd_create" 'I Clockid Flags))


(de set-time (Fd Flags New-Value Old-Value)
  (native "@" "timerfd_settime" 'I Fd Flags New-Value Old-Value))

  
(de get-time (Fd Curr-Value)
  (native "@" "timerfd_gettime" 'I Fd Curr-Value))


(class +Timerfd)
(dm T (Data)
  (=: fd (create CLOCK-MONOTONIC 0))
  (=: data Data))

(dm set-time> (Spec)
  # If 2/4 >= 1000 ms must use 1/3
  (struct *itimerspec-new 'N
          (cons (get Spec 1) 8)
          (cons (get Spec 2) 8)
          (cons (get Spec 3) 8)
          (cons (get Spec 4) 8))
  # Can also return old to avoid needing to call get-time
  (set-time (: fd) 0 *itimerspec-new 0))
  
(dm get-time> ()
  # current, put in old
  (get-time (: fd) 0 *itimerspec-old)
  (struct *itimerspec-old '(N . 4)))
  
(dm arm> (Spec)
  # Set timespec nz
  (set-time> This Spec))
  
(dm disarm> ()
  # Set timespec zero
  (set-time> This (0 0 0 0)))