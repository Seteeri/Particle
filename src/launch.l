(de fork-2 (Fn)
  (let Pid-1 (fork)
    (cond
      (Pid-1 (native "@" "waitpid" NIL Pid-1 0 0))
      ((not Pid-1) # Child Proc B
       (let Pid-2 (fork)
         (cond
           (Pid-2 (bye))
           ((not pid) # Child Proc C
            (Fn))
           (T (quit "fork=-1")))))
      (T (quit "fork=-1")))))


(de main ()
  
  (let Home (pack (pwd) "/src/")
        
      (fork-2 '(() (exec "pil" (pack Home "render/render.l") "-main" "+")))
      (fork-2 '(() (exec "pil" (pack Home "input/input.l")   "-main" "+")))
      (fork-2 '(() (exec "pil" (pack Home "ctrl/ctrl.l")     "-main" "+")))
      (fork-2 '(() (exec "pil" (pack Home "worker/worker.l") "-main" "+")))
      
      T)

  (bye))
  
#{
  (def '*proc-render)
  (def '*proc-model)
  (def '*proc-input)
  (def '*proc-ctrl)
  (def '*proc-work-1)
  (def '*proc-work-2)
  (def '*proc-work-3)
  (def '*proc-work-4)

  (pipe (call "taskset" "-pc" "0" (format *proc-work-1)) (read))
  #(prinl (pipe (call "taskset" "-pc" "1" (format *proc-work-2)) (read)))
  
  (pipe (call "taskset" "-pc" "1" (format *proc-model)) (read))
  
  (pipe (call "taskset" "-pc" "2" (format *proc-ctrl))  (read))
  (pipe (call "taskset" "-pc" "2" (format *proc-input)) (read))

  # Poss push all of CPU 2 to 3 with render?
  (pipe (call "taskset" "-pc" "3" (format *proc-render)) (read))))
}#
