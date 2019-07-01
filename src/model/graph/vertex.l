#Node tex dims are all 96x96
(def '*dims-tex-node (list 96 96))

(def '*color-node-default (list 0.5137255 0.5803922 0.60784316 1.0 0.5137255 0.5803922 0.60784316 1.0
				0.5137255 0.5803922 0.60784316 1.0 0.5137255 0.5803922 0.60784316 1.0))


(class +Vertex)
(dm T (Char       #Use to get offset, if char then data is char also
       UV
       RGBA
       MM)        #Rename to transforms? -> Nah, MM is output so intermediates are members of it
       
    (=: char Char)
    #Texture only
    (=: off-texel (* (- (: char) 1) 96 96))
    (=: uv UV)
    #Pixel only
    (=: rgba RGBA)
    #All
    (=: mm MM)
    #Reuse this, also use as cache for optimization
    #Or maintain lispy list of bytes?
    (=: ptr (native "@" "malloc" 'N 208)))


(dm update> ()

    (=: off-texel (* (- (: char) 1) 96 96))
    
    T)


(dm serialize-to-ptr> ()
    (let Off 0

	 #Use make/link with all of these?
	 
	 (write-matrix-to-ptr (: ptr) (: mm) 0)

	 (inc 'Off 64)
	 
	 (write-matrix-to-ptr (: ptr) (: rgba) 0)

	 (inc 'Off 64)
	 
	 (write-matrix-to-ptr (: ptr) (: uv) 0)

	 (inc 'Off 64)
	 
	 (struct (: ptr) 'N
		 (cons (: off-texel) 4)
		 (cons 96 4) #x-sz
		 (cons 96 4) #y-sz
		 (cons 0 4) #flag
		 ))
    (: ptr))


(dm serialize-to-lst> ()
    #{
    off-texel-texture = (* (- Char-or-Code 1) 96 96)
    uv = metrics
    rgba = floats
    mm = floats
    }#

    (serialize-to-ptr> This)

    (struct (: ptr) '(B . 208)))


(dm serialize-to-str> ()
    (pack "("
	  (glue " " (let L NIL
			 (for N (serialize-to-lst> This)
			      (push 'L (format N)))
			 (flip 'L)
			 L))
	  ")"))
