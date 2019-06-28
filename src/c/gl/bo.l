#(symbols 'gl 'pico)


(class +Buffer-Object)
(dm T (Size
       Target
       Index
       Mapped-persistent
       Usage
       Data)
    #Usage = dynamic-draw or stream-draw
    #Data  = pointer

    (=: buffer (car (gen-buffers 1)))
    
    (=: size-b   Size)
    (=: target Target)
    (=: index-b   Index)
    (=: mapped-persistent Mapped-persistent)
    (=: bind-b (make-bind Target Index))
    
    (bind-b> This)
    
    (=: ptr 0)
    
    (if Mapped-persistent
	(=: ptr (map-buffer-range-2 Target
				    0
				    Size
				    (if Data Data 0)))
	(buffer-data Target
		     Size
		     (if Data Data 0)
		     Usage))
    
    (bind-b> This))


(de make-bind (Target
	       Index)
    (cond ((or (= Target DRAW-INDIRECT-BUFFER)
	       (= Target ELEMENT-ARRAY-BUFFER))
	   '((Target Buffer)
	     (bind-buffer Target Buffer)))
	  ((= Target TEXTURE-BUFFER)
	   '((Target Buffer)
	     # Are both needed?
	     (bind-texture Target Buffer)
	     (bind-buffer  Target Buffer)))
	  (T
	   '((Target Buffer Index)
	     (bind-buffer-base Target
	      Index
	      Buffer)))))


(dm bind-b> (Buffer)
    ((: bind-b) (: target) (: buffer) (: index-b)))


(de map-buffer-range-2 (Target
		        Offset
		        Size
		        Data)
    #Makes buffer immutable compared to calling glBufferData
    #This is required for mapping    
    (buffer-storage Target
		    Size
		    Data
		    MAP-BUFFER-RANGE-ACCESS)
    (map-buffer-range Target
		      Offset      #offset
		      Size
		      MAP-BUFFER-RANGE-ACCESS))


(def 'MAP-BUFFER-RANGE-ACCESS (| (hex "0002") (hex "0040") (hex "0080")))
