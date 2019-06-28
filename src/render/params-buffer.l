#{
- These are determined ahead of time
- To be completely dynamic:
- Free/Alloc GL memory
- Free/Alloc mmap/memfd
- Technically, all of it can be done dynamically
- Use these as baseline
}#


(class +Params-Buffer)
(dm T (Name
       Target
       Size
       Index-cs
       Index-vs
       Type-format)
    (=: name Name)
    (=: target Target)
    (=: size Size)
    (=: index-cs Index-cs)
    (=: index-vs Index-vs)
    (=: type-format Type-format))


(def '*params-buffer (list (new '(+Params-Buffer) "atomic-counter"
				'atomic-counter-buffer	
				(* 4 6)  #6 ints/params
				4 -1
				NIL)
			   
			   (new '(+Params-Buffer) "draw-indirect"
				'draw-indirect-buffer 
				(* 4 6)  #6 ints/params
				-1 -1
				NIL)

			   (new '(+Params-Buffer) "element"
				'element-array-buffer 
				(* 4 6)  #4 bytes/int * 6 ints or indices
				-1 -1
				NIL)
			   
			   (new '(+Params-Buffer) "nodes"
				'shader-storage-buffer 
				(/ 134217728 4)
				2 3
				NIL)

			   (new '(+Params-Buffer) "projview"
				'uniform-buffer
				(* (+ 16 16) 4)
				0 0  #cs-in (cache), vs-in (raster)
				NIL)

			   (new '(+Params-Buffer) "texture-glyphs"
				'texture-buffer 
				(/ 134217728 4)
				-1 -1
				'rgba8)

			   (new '(+Params-Buffer) "vertices"
				'uniform-buffer
				(* 16 4)
				1 1
				NIL)))
