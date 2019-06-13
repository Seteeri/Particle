(let* ((cursor nil)
   (ls-chars (make-array size
             :element-type 'character
             :fill-pointer nil
             :adjustable nil))
   (dlist (dlist:dlist))
   (ls ())
   (ht-chars (make-hash-table :size size)))
  
  (loop
 :for i :from 0 :below size
 :for code-char = (code-char (mem-aref (inc-pointer ptr i) :char))
 :do (progn
       (setf (gethash i ht-chars) i)
       (push code-char ls)
       (dlist:dlist-push code-char dlist :at-end t)
       (setf (aref ls-chars i) code-char)))

  ;;;;;;;;;;;;;;;;;;
  ;; Iteration Speed

  ;; Store inidices to prev/next item in hash-table
  ;; On delete/insert, check indices and update
  ;;
  ;; Order in data structure is not necessary,
  ;; although it may provide a natural timeline of operations

  ;; LinkedHashMap = maintain order
  ;; Doubly linked list + hash table
  ;; Create dlnode and store in hash table instead

  ;; 0 1 2 3 4 5 6; 3 = 2 4
  ;; 0 1 2 - 4 5 6; 2 = 1 4
  
  ;; 8815
  ;; loops = 8 815 000
  ;; (format t "Length: ~a~%" (length ls-chars))
  
  ;; =
  ;; 0.016
  ;; (time
  ;;  (dotimes (j 1000)
  ;; 	 (loop
  ;; 	    :for key :being :the :hash-keys :of ht-chars
  ;; 	    :using (hash-value value))))
  
  ;; 2X slower
  ;; 0.051
  ;; (time
  ;;  (dotimes (j 1000)
  ;; 	 (dolist (i ls)
  ;; 	   i)))
  
  ;; 4X slower - aim for speed no slower than 4X since average effective core count is 4
  ;; 0.079
  ;; @2**17 = 0.002 sec = 2 ms so hash would take roughly 0.5 ms
  ;; (time
  ;;  (dotimes (j 15) ;; ~2**17 = 131072
  ;; 	 (dlist:dodlist (i dlist nil)
  ;; 			i)))

  ;; Baseline
  ;; 0.018
  ;; (time
  ;;  (dotimes (j 1000)
  ;; 	 (dotimes (i size)
  ;; 	   (aref ls-chars i))))      
