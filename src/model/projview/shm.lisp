(in-package :protoform.model)

(defun copy-mat-proj-to-shm ()
  (set-matrix (ptr *shm-projview*)
	      (mat-proj *projview*)
	      0))

(defun copy-mat-view-to-shm ()
  (set-matrix (ptr *shm-projview*)
	      (mat-view *projview*)
	      16))

(defun enqueue-mat (fn-update mat offset)
  (sb-concurrency:enqueue
   (list *channel*
	 *shm-projview*
	 (lambda ()
	   (funcall fn-update)
	   (let ((arr (marr (mtranspose mat))))
	     (pack:pack "<16f"
			(aref arr 0)  (aref arr 1)  (aref arr 2)  (aref arr 3)
			(aref arr 4)  (aref arr 5)  (aref arr 6)  (aref arr 7)
			(aref arr 8)  (aref arr 9)  (aref arr 10) (aref arr 11)
			(aref arr 12) (aref arr 13) (aref arr 14) (aref arr 15))))
	 offset)
   *queue-shm*))

(defun enqueue-mat-proj ()
  (enqueue-mat #'update-mat-proj
	       (mat-proj *projview*)
	       0))

(defun enqueue-mat-view ()
  (enqueue-mat #'update-mat-view
	       (mat-view *projview*)
	       (* 16 4)))
