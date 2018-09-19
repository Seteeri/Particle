;; sbcl --dynamic-space-size 4096 --load array-loop-benchmark.lisp --eval "(benchmark)"
;; sbcl --dynamic-space-size 4096 --load array-loop-benchmark.lisp

(ql:quickload 'trivial-benchmark)

(defun foo (x)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (simple-array fixnum (*)) x))
  (the (simple-array fixnum (*)) x)
	(dotimes (i (the fixnum (length x)))
	  (declare (type fixnum i))
      (the fixnum i)
	  (the fixnum (aref x i))))

(cffi:defcfun "memcpy" :void
    (dest :pointer)
    (src :pointer)
    (n :uint))
    ;;(n (:pointer :uint)))

;; Lisp: 1.3 ms = ~12X = 192MB/frame
;; C:    0.6 ms = ~27X = 432MB/frame
;; For memcpy optimal point...
;; https://stackoverflow.com/questions/21038965/why-does-the-speed-of-memcpy-drop-dramatically-every-4kb
;; http://nadeausoftware.com/articles/2012/05/c_c_tip_how_copy_memory_quickly

(defun main ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))

  ;;(x (make-array (* 16 1024 1024)
  ;;		       :element-type 'fixnum
  ;;		       :fill-pointer nil
  ;;		       :initial-element 0))

  (let* ((dest (cffi:foreign-alloc :uint :count (* 16 1024 1024) :initial-element 0))
         (src (cffi:foreign-alloc :uint :count (* 16 1024 1024) :initial-element 0)))
         ;;(size (cffi:foreign-alloc :uint :initial-element (* 16 1024 1024))))
        
      (benchmark:with-timing (14)
        (memcpy dest
                src
                (the (unsigned-byte 32) (* 16 1024 1024))))
        
       (cffi:foreign-free src)
       (cffi:foreign-free dest))
    
  t)

(main)

;; loop - 300  : 0.5 micro
;; copy - 1.2  : 0.24 ms
