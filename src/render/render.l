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

#Create and change to namespace model
(symbols 'render 'pico)

# C library paths
(def 'PATH-MATHC "/home/user/quicklisp/local-projects/protoform/ext/mathc/libmathc.so")
(def 'PATH-GLYPHS "/home/user/quicklisp/local-projects/protoform/src/glyphs")
# UDS paths
(def 'PATH-UDS-RENDER "/tmp/protoform-render.socket")

# Set math precision to 6 decimals
(scl 6)

# Wrapper libraries
(load "/home/user/quicklisp/local-projects/protoform/src/c/mathc.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/os.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/sock.l")
# Lisp libraries
#...

(de print-render (Ctx
		  Str)
    (println "RENDER" *Pid Ctx Ctr))

(de init-render ()
    
    #{
    - Model can modify these during runtime through wire
    - Model will send (memcpy dest size offset)
    - Render will look up symbol/Mmap, get ptr, memcpy
    }#
    #(setq *mmap-atomic-counter (init-mmap-atomic-counter))
    #(setq *mmap-draw-indirect  (init-mmap-draw-indirect))
    #(setq *mmap-vertices       (init-mmap-vertices))
    #(setq *mmap-element        (init-mmap-element))
    #(setq *mmap-projview       (init-mmap-projview))
    #(setq *mmap-tex-glyphs     (init-mmap-tex-glyphs))
    
    NIL)

(de run-render ()
    #After connecting, start processing messages
    #Render will send frame start and wait for frame end
    #Up to model to perform time management

    T)

(symbols 'pico)
(de main ()
    (render~init-render)
    (render~run-render)
    (println 'Exiting...)
    (bye))
