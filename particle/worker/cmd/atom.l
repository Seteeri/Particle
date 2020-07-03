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

    (make-pair-str Dat Lay)))


(de cmd-make-nil (Ks)
  #(prinl-info "cmd-make-nil")
  (make-pair-str NIL 'x))

  
(de cmd-make-num (Ks)

  (println "cmd-make-num")

  (make-pair-str (format (char Ks)) 'x))
