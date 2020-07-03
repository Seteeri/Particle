###################
# ATOM/STR <-> LIST

(de cmd-chop/split ()

  (println 'cmd-chop/split)

  #{
  
    If atom/str, chop
    If list, split with reg
      Ex: make sp, push to reg, do cmd
      Mov start or end?
  
  }#
  
  (cond
  
    ((a-atom?> *ptr)
     (chop-atom car))

    ((b-atom?> *ptr)
     (chop-atom cdr))
     
    ((a-pair?> *ptr)
     (split-pair-a))
      
    ((b-pair?> *ptr)
     (split-pair-b))))


(de cmd-glue ()

  (println 'cmd-glue)
  
  #{
  
    Glue from pt fwd, with reg
      Ex: make sp, push/mov to reg, do cmd
      Mov start or end? end...can use ptr cmd
      
  }#
  
  (cond
  
    ((a-atom?> *ptr)
      # Get its pair - glue fwd
     (glue-atom-a))
      
    ((b-atom?> *ptr)
      # Glue list
      # Get current list or glue rev?
     (glue-atom-b))
    
    ((or (a-pair?> *ptr)
         (b-pair?> *ptr))
     # Glue fwd
     (glue-pair))))
      

###############
# ATOM <-> ATOM

(de cmd-conv ()
  
  #{
  
    t-sym -> i-sym = intern
    i-sym -> t-sym = sym
    
    num <-> t-sym = format
    
    Note, can only intern str:
    num -> t-str -> i-sym
    
    How to do floating-point?
    - Handle as str + read
    
    'any/'str = str -> data
  
  }#

  T)
  
