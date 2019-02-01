(in-package #:protoform.analyzer-dep)

;; Move to own package

(defparameter *as-fns* (make-hash-table :size 64))
(defparameter *as-syms* (make-hash-table :size 64))

(defparameter *sa-root* nil)

(defparameter *debug-analyzer* nil)

(defclass node-dep ()
  ((data :accessor data :initarg :data :initform nil)))

;; from declare data
(defun get-fn-rw (value)
  (cdr (second value)))

(defun analyze-file (path-lisp path-tasks fn-root)

  ;; PROCESS:
  ;; 1. Sort into hashtables - this should not be done everytime! -> generate offline, save to lisp file
  ;;    1. Functions : declare
  ;;    2. Symbols : fns-read, fns-write
  ;; 2. Pass function to start building dependency graph
  ;;    1. Build graph and sort into levels: vertical = serial, horizontal = parallel
  ;; 3. Derive levels from dependency graph
  ;;    - Could keep DAG and submit tasks during traversal?
  ;;    - Could only submit tasks that have no child nodes (and mark as submitted)
  ;;    - This can be memoized
  ;;    - Do breadth first traversal

  ;; Given a set of fns, if writes from one fn overlap with reads from another fn then cannot parallelize
  ;; However, reads from one fn can overlap with reads from another fn
  ;; access/modify; load/store
  
  ;; set-digraph = :r nil, :w digraph [single]
  ;; set-node-pointer = :r digraph, :w digraph [single]
  ;; a-function = :r digraph [unlimited]
  ;; - in this case, need to define intermediate variables to determine dependency
  ;; - ...instead of globals, store in another structure to manage namespaces...package?

  ;; MT:
  ;; Do search, add nodes to work queue, while threads grab from work queue
  
  (with-input-from-string (stream (read-file-string path-lisp))
    (loop
       :with eof-marker := :eof
       :for data := (read stream nil eof-marker)
       :until (eq data eof-marker)
       :do (process-data data)))

  (when nil
    (maphash (lambda (key value)
	       (format t "~S = ~S~%" key value))
	     *as-syms*))

  ;; (let ((digraph (digraph:make-digraph))
  ;; 	(tasks (list))) ; could use skip list
  ;; (recurse-node-dep digraph
  ;; 		      fn-root
  ;; 		      tasks
  ;; 		      0)

  (multiple-value-bind (digraph tasks)
      (iter-node-dep fn-root)
    
    (format t "~a~%" tasks)
    
    (when t
      (digraph.dot:draw digraph
  			:filename (str:concat (str:substring 0 -5 (file-namestring path-lisp))
					      ".png")
			:format :png))
    
    ;; (sb-ext:exit)
    
    (when t
    
      (with-open-file (stream path-tasks
			      ;; :external-format charset:iso-8859-1
			      :direction :output
			      :if-exists :overwrite
			      :if-does-not-exist :create)
	(format stream "#(~%")
	(loop     ;; Reverse iterate
	   :for i :from (1- (length tasks)) :downto 0
	   :for task := (aref tasks i)
	   :do (progn
		 (format stream "#(~%")
		 (loop
		    :for task-2 :across task
		    :do (format stream "~S~%" (data task-2)))
		 (format stream ")~%")))
	(format stream ")~%")))
    
    (values digraph
	    *sa-root*
	    tasks)))


(defun recurse-node-dep (digraph fn tasks level &optional node-last)
  (format t "[recurse-...] ~v@{~A~:*~}"
	  level
	  "  ")
  (format t "~a -> #'~a~%"
	  (if node-last
	      node-last
	      nil)
	  (symbol-name fn))
  
  (let* ((node-w (make-instance 'node-dep :data fn)))
    (digraph:insert-vertex digraph
    			   fn)
    ;; Save root
    (when (not *sa-root*)
      (setf *sa-root* node-w))
    
    (when node-last
      (digraph:insert-edge digraph
			   node-last
			   fn))

    ;; Create level as needed, then push it
    (let ((level-task nil))
      (if (= (length tasks) level)
	  (progn
	    (setf level-task (list))
	    (push level-task
		  tasks))
	  (setf level-task (nth level tasks)))
      (push node-w
	    level-task))
    
    ;; read syms -> write funs -> create dep node for each fun
    (destructuring-bind (reads writes) (get-fn-rw (gethash fn *as-fns*))
      (dolist (sym-read (cdr reads))
	(dolist (fn-write (second (gethash sym-read *as-syms*)))
	  (when (not (eq fn fn-write))
	    (recurse-node-dep digraph
			      fn-write
			      tasks
			      (1+ level)
			      fn)))))))

(defun iter-node-dep (fn-root)
  (let* ((digraph (digraph:make-digraph))
	 (arr-root (make-array 1 :adjustable t :fill-pointer 0))
	 (tasks (make-array 1 :adjustable t :fill-pointer 1 :initial-contents (list arr-root))))
    (loop
       :with fns := (list (list fn-root
				0
				nil))
       :for fn-args := (pop fns)
       :while fn-args
       :do (destructuring-bind (fn level fn-last)
	       fn-args
	     
	     (format t "[iter-...] ~v@{~A~:*~}"
		     level
		     "  ")
	     (format t "~a -> #'~a~%"
		     (if fn-last
			 fn-last
			 nil)
		     (symbol-name fn))

	     (format t "~a, ~a~%" fn level)
	     
	     (let* ((node-w (make-instance 'node-dep :data fn)))
	       (digraph:insert-vertex digraph
    				      fn)
	       ;; Save root
	       (when (not *sa-root*)
		 (setf *sa-root* node-w))
	       
	       (when fn-last
		 (digraph:insert-edge digraph
				      fn-last
				      fn))
	       
	       (vector-push-extend node-w
				   (aref tasks level))
	       
	       ;; read syms -> write funs -> create dep node for each fun

	       ;; if level exists get it
	       (let ((deps nil)
		     (list-deps (if (>= (1+ level) (length tasks))
				    (make-array 1 :adjustable t :fill-pointer 0)
				    (aref tasks (1+ level)))))
		 
		 (destructuring-bind (reads writes) (get-fn-rw (gethash fn *as-fns*))
		   (dolist (sym-read (cdr reads))
		     (dolist (fn-write (second (gethash sym-read *as-syms*)))
		       (when (not (eq fn fn-write))
			 ;; all are on the same level, aka run parallel
			 (setf deps t)
			 ;; push to list
			 ;; need to make sure this node hasn't been processed if multiple edges
			 (push (list fn-write
				     (1+ level)
				     fn)
			       fns)))))

		 ;; Found new level so add to list
		 (when (and deps
			    (>= (1+ level) (length tasks)))
		   (vector-push-extend list-deps tasks))))))
    
    (values digraph
	    tasks)))

(defun process-data (data)
  (when (listp data)
    (when *debug-analyzer*
      (format t "list | ~S...~%" (length data)))
    (when (eq (first data) 'defun)
      (let* ((name-fn (second data))
	     (kw-fn (intern (symbol-name name-fn) "KEYWORD")))
	;; Process fn:declare
	(setf (gethash kw-fn *as-fns*)
      	      (fourth data))
	;; Process syms:r,w fns
	(destructuring-bind (reads writes)
      	    (cdr (second (fourth data)))
      	  (process-rw kw-fn reads :r)
      	  (process-rw kw-fn writes :w))))
    
    (process-list (cdr data))))

(defun process-rw (kw-fn list type)
  (dolist (sym (cdr list))
    (let ((kw (intern (symbol-name sym) "KEYWORD"))
	  (fns nil))
      (if (gethash kw *as-syms*)
	  (setf fns (gethash kw *as-syms*))
	  (progn
	    (setf fns (list nil nil))
	    (setf (gethash kw *as-syms*) fns)))
      (if (eq type :r)
	  (push kw-fn (first fns))
	  (push kw-fn (second fns))))))
    
(defun process-list (list)
  (dolist (data list)
    (process-data data)))

#|
(defun process-data (data)
  (if (listp data)
      (progn
	(when *debug-analyzer* (format t "list | ~S...~%" (length data)))
	(when (eq (first data) 'defun)
	  (setf (gethash (second data) *as-fns*)
		(fourth data)))
	(process-list (cdr data)))
      (progn
	(when *debug-analyzer* (format t "atom | ~S~%" data))
	(when (and (symbolp data)
		   (not (find-symbol (string data) 'common-lisp)))
		   ;; (str:starts-with? "*" (string data)))
	  (digraph:insert-vertex *as-digraph* data)
	  (when *as-sym-last*
	    (digraph:insert-edge *as-digraph* *as-sym-last* data))
	  t))))
|#

#|
(defparameter *debug-analyzer* t)

;; As per ANSI SPEC
(defparameter *symbols-special-forms*
'(block      let*                  return-from
catch      load-time-value       setq  
eval-when  locally               symbol-macrolet
flet       macrolet              tagbody
function   multiple-value-call   the
go         multiple-value-prog1  throw   
if         progn                 unwind-protect
labels     progv
let        quote))

(defun process-data (data)
(if (listp data)
(let ((first (first data)))
(cond ((member first *symbols-special-forms*)
(when *debug-analyzer* (format t "list, special | ~S...~%" first))
(dispatch-special-form-handlers data)
t)
((macro-function first)
(when *debug-analyzer* (format t "list, macro   | ~S...~%" first))
;; Can't macroexpand
;; Have to add custom parsers for macros?
;; (format t "~a~%" (sb-cltl2:macroexpand-all data))
;; (process-list (sb-cltl2:macroexpand-all data))
(format t "macro: ~a~%" data)
(process-list data))
(t 
(when *debug-analyzer* (format t "list, normal  | ~S...~%" first))
(process-list data))))
(when *debug-analyzer* (format t "atom          | ~S~%" data))))

(defun process-list (list)
(dolist (data list)
(process-data data)))

(defun dispatch-special-form-handlers (form)
(let ((first (first form)))
(cond ((or (eq first 'labels)
(eq first 'let)
(eq first 'let*)
(eq first 'flet)
(eq first 'macrolet)
(eq first 'symbol-macrolet))
;; (format t "nth 0... \"~a\"~%" (nth 0 form))
;; (format t "nth 1... \"~a\"~%" (nth 1 form))
;; (format t "dispatch 1... \"~a\"~%" (nth 2 form))
;; (format t "dispatch 2... \"~a\"~%" (first (nthcdr 3 form)))
;; second item = lists
(process-list (nth 1 form))
(process-list (nthcdr 2 form)))
(t
(process-list (cdr form))))))
|#
