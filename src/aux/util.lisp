(in-package #:protoform.aux)

(defun align-size (size &optional (boundary 4))
  (+ size (- boundary (mod size boundary))))

(declaim (inline normalize))
(defun normalize (a b c d)
  (v/ (vec4 a b c d) (vlength (vec3 a b c))))

(declaim (inline rad-to-deg))
(defun rad-to-deg (rad)
  (/ (* rad 180.0) pi))

(declaim (inline deg-to-rad))
(defun deg-to-rad (deg)
  (/ (* deg pi) 180.0))

(declaim (inline read-file-string))
(defun read-file-string (path)
  (with-open-file (stream path :external-format :utf-8)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun floats-epsilon-equal-p (f1 f2)
  (< (abs (- f1 f2)) SINGLE-FLOAT-EPSILON))

(defun print-monotonic-time ()
  (let ((time (osicat:get-monotonic-time)))
    (format t "Model: ~8$ ms~%" (* time 1000))))

(defun print-hash-table (ht)
  (maphash (lambda (key value)
	     (fmt-model t "" "~S : ~S~%" key value))
	   ht))
