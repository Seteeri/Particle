(in-package :protoform.model)
  
;; (defun clean-up-handles-shm ()
;;   (loop 
;;      :for key :being :the :hash-keys :of *handles-shm*
;;      :using (hash-value mmap)
;;      :do (cleanup-mmap mmap)))

;; (defun init-handles-shm ()
;;   (loop
;;      :with handles-shm := (make-hash-table :size 6 :test 'equal)
;;      :for params :in *params-shm*
;;      :do (destructuring-bind (target
;; 			      name
;; 			      path
;; 			      size
;; 			      bind-cs
;; 			      bind-vs
;; 			      count-buffer
;; 			      flag-copy
;; 			      &rest rest)
;; 	     params
;; 	   (let ((mmap (init-mmap path
;; 				  size
;; 				  t ; create - replace these types with symbols
;; 				  :data (make-array size
;; 						    :element-type '(unsigned-byte 8)
;; 						    :initial-element (coerce 0 '(unsigned-byte 8))))))
;; 	     (setf (gethash name handles-shm) mmap)
;; 	     (fmt-model t "init-handle-shm" "~S, ~S bytes~%" path size)))
;;      :finally (return handles-shm)))

;; rename mmap -> shm
(defun init-shm (name)
  (destructuring-bind (target
		       name
		       path
		       size
		       bind-cs
		       bind-vs
		       count-buffer
		       flag-copy
		       &rest rest)
      (getf *params-shm* name)
    (init-mmap path
	       size
	       t ; create - replace these types with symbols
	       :data (make-array size
				 :element-type '(unsigned-byte 8)
				 :initial-element (coerce 0 '(unsigned-byte 8))))))

(defun init-shm-projview ()
  (let ((shm (init-shm :projview)))
    (setf *shm-projview* shm)
    (update-mat-proj)
    (update-mat-view)
    (copy-mat-proj-to-shm)
    (copy-mat-view-to-shm)
    shm))

(defun init-shm-nodes ()
  (init-shm :nodes))

(defun init-shm-atomic-counter ()
  (init-shm :atomic-counter))

(defun init-shm-vertices ()
  ;; top right, bottom right, bottom left, top left
  ;;
  ;; 3---0
  ;; | / |
  ;; 2---1
  ;;
  ;; ccw: 0 2 1 0 3 2
  (let ((shm (init-shm :vertices)))
    (with-slots (ptr size)
	shm
      (let ((data (make-array (* 4 4)
			      :element-type 'single-float
			      :initial-contents (list 1.0  1.0  0.0  1.0
						      1.0  0.0  0.0  1.0
						      0.0  0.0  0.0  1.0
						      0.0  1.0  0.0  1.0))))
	(dotimes (i (length data))
	  (setf (mem-aref ptr :float i)
		(aref data i)))))
    shm))

(defun init-shm-element ()
  (let ((shm (init-shm :element)))
    (with-slots (ptr size)
	shm
      (let ((data (make-array 6
      			      :element-type '(unsigned-byte 32)
      			      :initial-contents (list 0 2 1 0 3 2))))
	(dotimes (i (length data))
	  (setf (mem-aref ptr ::uint i)
		(aref data i)))))
    shm))

(defun init-shm-draw-indirect ()
  (let ((shm (init-shm :draw-indirect)))
    (with-slots (ptr size)
	shm
      (let ((data (make-array 5
      			      :element-type '(unsigned-byte 32)
      			      :initial-contents (list 6 *inst-max* 0 0 0))))
	(dotimes (i (length data))
	  (setf (mem-aref ptr ::uint i)
		(aref data i)))))
    shm))

(defun init-shm-texture-glyphs ()

  ;; Could convert lisp data straight to bytes...
  ;; Load into textures for now...

  (let ((shm (init-shm :texture)))
    (with-slots (ptr size)
	shm
      (loop
	 :for code :from 1 :to 255
	 :with msdf-glyphs-path := (merge-pathnames #P"glyphs-msdf/" (asdf:system-source-directory :protoform))
	 :with i := 0
	 :for lisp-path := (merge-pathnames (make-pathname :name (format nil "~a-data" (write-to-string code))
							   :type "lisp")
					    msdf-glyphs-path)
	 :do (loop 
		:for c :across (read-from-string (read-file-string lisp-path))
		:do (progn
		      (setf (mem-aref ptr :unsigned-char i) c)
		      (incf i)))))
    shm))
