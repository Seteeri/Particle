#(symbols 'gl 'pico)


(def 'MAP-BUFFER-RANGE-ACCESS (| (hex "0002") (hex "0040") (hex "0080")))


(class +Buffer-Object)
(dm T (Size
       Buffer
       Target
       Index
       Fn-bind       
       Mapped-persistent

       Usage #dynamic-draw or stream-draw
       Data) pointer

    (=: size   Size)
    (=: buffer (car (gen-buffers 1)))
    (=: target Target)

    (=: index   Index)
    (=: bind (make-bind Target Index))
    (=: mapped-persistent Mapped-persistent)

    ((: bind) (: buffer))
    
    (if mapped-persistent
	(map-buffer-range Target
			  0
			  Size
			  (if Data Data 0))
	(buffer-data Target
		     Size
		     (if Data Data 0)
		     Usage))

    ((: bind) 0))


(de make-bind (Target
		  Binding-layout)
    (cond ((or (= Target 'draw-indirect-buffer)
	       (= Target 'element-array-buffer))
	   '((Buffer)
	     (bind-buffer Target Buffer)))
	  ((= Target 'texture-buffer)
	   '((Buffer)
	     # Are both needed?
	     (bind-texture Target Buffer)
	     (bind-buffer  Target  Buffer)))
	  (T
	   '((Buffer)
	     (bind-buffer-base Target
			       Binding-layout
			       Buffer)))))


(de map-buffer-range (Target
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
		      0      #offset
		      Size
		      MAP-BUFFER-RANGE-ACCESS))


#{
(defun update-binding-buffer (buffer index)
  (with-slots (fn-bind buffers) buffer
    (funcall fn-bind (aref buffers index))))

(defun clean-up-buffer-object (buffer)
  ;; For each boa: bind, unmap, bind zero, delete
  (with-slots (buffers
	       fn-bind
	       mapped-persistent
	       target)
      buffer
    (loop 
       :for bo :across buffers
       :do (progn
	     (funcall fn-bind bo)
	     (when mapped-persistent
	       (gl:unmap-buffer target))
	     (funcall fn-bind 0)))
    (gl:delete-buffers buffers)))

(defun set-bo-draw-indirect (bo-indirect
			     count
			     prim-count
			     first-index
			     base-vertex
			     base-instance)

  ;; count: the number of elements to be rendered.
  ;; type: the type of the values in indices. Must be one of GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, or GL_UNSIGNED_INT.
  ;; indices: a byte offset (cast to a pointer type) into the buffer bound to GL_ELEMENT_ARRAY_BUFFER to start reading indices from.
  ;; primcount: the number of instances of the indexed geometry that should be drawn.
  ;; basevertex: a constant that should be added to each element of indices​ when chosing elements from the enabled vertex arrays.
  ;; baseinstance: the base instance for use in fetching instanced vertex attributes.
  
  (with-slots (count-buffers ptrs-buffer) bo-indirect
    (loop
       :for i :from 0 :below count-buffers
       :for ptr := (aref ptrs-buffer i)
       :do (progn
	     (setf (mem-aref ptr :uint 0) count)             ; count (# of elements to render, i.e. number of indices to render from ebo)
	     (setf (mem-aref ptr :uint 1) prim-count)        ; prim-count (# of instances)
	     (setf (mem-aref ptr :uint 2) first-index)       ; first-index  (# offset into indices vbo)
	     (setf (mem-aref ptr :int  3) base-vertex)       ; base-vertex (added to element indices, i.e. stride)
	     (setf (mem-aref ptr :uint 4) base-instance))))) ; base-instance


(defun set-bo-element (bo-element data)
  ;; Better way to do this...
  (with-slots (count-buffers ptrs-buffer) bo-element
    (loop
       :for i :from 0 :below count-buffers
       :for ptr := (aref ptrs-buffer i)
       :do (dotimes (k 6)
	     (setf (mem-aref ptr :uint k) (aref data k))))))
}#
