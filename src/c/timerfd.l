(symbols 'timerfd 'pico)

(local) (create)

# CLOCK-MONOTONIC 1

(de create (Clockid Flags)
  (native "@" "timerfd_create" 'I Clockid Flags))

(de set-time (Fd Flags New-Value Old-Value)
  (native "@" "timerfd_settime" 'I Fd Flags New-Value Old-Value))
  
(de get-time (Fd Curr-Value)
  (native "@" "timerfd_gettime" 'I Fd Curr-Value))