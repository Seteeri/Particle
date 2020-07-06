###################################
# MAKE (atomic prototype functions)

(de cmd-make-char (Ks)

  #{
    \r = ^M
    \n = ^J
    \t = ^I
  }#
  
  (let (Dat (cond ((= Ks ksd~Return) "^J")
                   ((= Ks ksd~Tab) "^I")
                   (T (char Ks)))
        Lay 'x)

    # Push to cmd list
    # Make class
    # Put properties

    (make-str Dat Lay)))


(de cmd-make-nil (Ks)
  #(prinl-info "cmd-make-nil")
  (make-str NIL 'x))

  
(de cmd-make-num (Ks)

  (println "cmd-make-num")

  (make-str (format (char Ks)) 'x))
