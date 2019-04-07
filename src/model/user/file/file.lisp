(in-package :protoform.model)

(defun load-file-to-nodes-ptree (ptree)
  ;; Submit tasks to model or add to ptree directly

  ;; (ptree-fn 'area   '(width height) (lambda (w h) (* w h))       tree)
  ;; (ptree-fn 'width  '(border)       (lambda (b)   (+ 7 (* 2 b))) tree)
  ;; (ptree-fn 'height '(border)       (lambda (b)   (+ 5 (* 2 b))) tree)
  ;; (ptree-fn 'border '()             (lambda ()    1)             tree)
  
  (ptree-fn 'lftn-in
	    '()
	    (lambda ()
	      (open (make-pathname :directory '(:absolute
						"home"
						"user"
						"quicklisp"
						"local-projects"
						"protoform")
				   :name "README" :type "md")
		    :external-format :utf-8))
	    ptree)

  (ptree-fn 'lftn-origin
  	    '()
  	    (lambda ()
	      (get-origin-from-node-pos *node-ptr-main*))
  	    ptree)
  
  (ptree-fn 'lftn-lcf
	    '(lftn-in lftn-origin)
	    (lambda (in origin)
    	      (funcall #'load-chunk-file
	    	       in
	    	       0
	    	       4096
	    	       0
	    	       0
		       origin
		       origin))
	    ptree)

  'lftn-lcf)


(defun load-chunk-file (in
			start
			length
			;; create structure for below
			index-data
			index-char
			baseline
			pen)
  ;; Read data
  ;; Return params for next frame
  (let* ((data (make-string length))
	 (pos (read-sequence data in)))
    (fmt-model "load-chunk-file" "pos = ~a~%" pos)
    (when (plusp pos)
      ;; Enqueue in async so it can create nodes
      ;; When it is done with the chunk, it will request another here
      ;; And this will repeat...
      (let* ((in-2 in)
	     (start-2 start)
	     (pos-2 pos)
	     (data-2 data)
	     (task (make-instance 'task
				  :id 'load-char-from-file
				  :fn-play (lambda (task)				   
					     (load-char-from-file in-2
								  start-2
								  pos-2
								  data-2
								  index-data
								  index-char
								  baseline
								  pen)))))
	;; (sb-concurrency:send-message
	;;  *mb-async*
	;;  task)
	(enqueue-task-async task)
	task))))

(defun load-char-from-file (in
			    start
			    length
			    data
			    ;; create structure for below
			    index-data
			    index-char
			    baseline
			    pen)

  ;; pass loop count
  (dotimes (i 4)
    
    (when (>= index-data (length data))
      ;; Read new chunk
      (sb-concurrency:send-message
       *mb-async*
       (make-instance 'task
		      :id 'load-chunk-file
		      :fn-play (lambda (task)
				 
				 (setf (gethash 'load-chunk-file *tasks-active*) task)
				 
    	      			 (funcall #'load-chunk-file
					  in
					  (+ start length)
					  4096
					  0 ; reset
					  index-char
					  baseline
					  pen))))
      (return-from load-char-from-file))
    
    (let ((char (schar data index-data)))

      (when (char-equal char #\Nul)
	;; EOF
	;; (format t "FOUND EOF~%")
	(close in)
	(remhash 'load-chunk-file *tasks-active*)
	(remhash 'load-char-from-file *tasks-active*)
	(return-from load-char-from-file))
      
      ;; calc pos relative to line rather than prev char
      (when (char-equal char #\Newline)
	(move-pen-newline pen baseline)
	(setf index-char 0))
      
      (make-char-to-node
       char
       (v+ pen
	   (vec3 (* 9.375 +scale-msdf+ *scale-node* index-char)
		 0
		 0)))
    
      (incf index-data)
      (incf index-char)))
	
  ;; Continue to consume data
  ;; Depends how many chars to create at once
  
  (let ((task (make-instance 'task
			     :id 'load-char-from-file
			     :fn-play (lambda (task)			    
					(load-char-from-file in
							     start
							     length
							     data
							     index-data
							     index-char
							     baseline
							     pen)))))
    ;; Must do this here so a subsequent stop task can stop the new task
    (setf (gethash 'load-char-from-file *tasks-active*) task)
    ;; (format t "[load/make...] NEW TASK = ~a~%" task)
    task))

;; Only one of these tasks are running
(defun stop-task-load-char-from-file (event)
  (stop-task-mb 'load-chunk-file)
  (stop-task 'load-char-from-file))

(defun pause-task-load-char-from-file (event)
  (pause-task-mb 'load-chunk-file)
  (pause-task 'load-char-from-file))

(defun resume-task-load-char-from-file (event)
  ;; (resume-task-mb 'load-chunk-file)
  (resume-task 'load-char-from-file))

;; (with-open-file (in filename)
;;   (let ((scratch (make-string 4096)))
;;     (loop for read = (read-sequence scratch in)
;;        while (plusp read) sum read)))


(defun load-file-to-nodes ()

  ;; PROBLEM:
  ;; - each char dependent on previous being drawn
  ;; - order not guaranteed since later char might finish before earlier char
  ;; SOLUTION:
  ;; - pp: init-node; calc positions based on char index
  ;; - insert vertices over all || insert tree over all
  ;; - once:update pointer last (or after each node)
  ;;   - attach ptr
  ;;   - advance ptr
  (when-let ((in (open (make-pathname :directory '(:absolute
						   "home"
						   "user"
						   "quicklisp"
						   "local-projects"
						   "protoform")
				      :name "README" :type "md")
		       :external-format :utf-8)))
	    (sb-concurrency:send-message
	     *mb-async*
	     (make-instance 'task
			    :id 'load-chunk-file
			    :fn-play (lambda (task)
				       
				       (setf (gethash 'load-chunk-file *tasks-active*) task)

				       ;; get origin in load-file-to-nodes, not lambda
    	      			       (funcall #'load-chunk-file
						in
						0
						4096
						0
						0
						(get-origin-from-node-pos *node-ptr-main*)
						(get-origin-from-node-pos *node-ptr-main*)))))))