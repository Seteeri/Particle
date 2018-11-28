(in-package #:protoform.model)

;; (when nil
;;   (let ((time (osicat:get-monotonic-time)))
;;     (format t "Model: ~8$ ms~%" (* (- time *time-last*) 1000))
;;     (setf *time-last* time)))

(defun submit-receive-graph (tasks)
  (loop
     :for nodes :across tasks
     :do (loop
	    :for node :across nodes
	    :do (progn
		  (fmt-model t "submit-receive-graph" "task: ~a~%" (string node))
		  (submit-task *channel*
			       (symbol-function (find-symbol (string node)
							     :protoform.model))))
	    :finally (dotimes (i (length nodes))
		       (receive-result *channel*)))))

(defun mapc-breadth-first-2 (function digraph start-vertex)
  "Apply `function` to the vertices of a breadth-first traversal of `digraph`.
  Returns `nil`.
  Vertices are processed in breadth-first order, beginning at `start-vertex`.
  Cycles in the graph will not be traversed into.
  "
  (let ((seen nil)
        (remaining nil))
    (labels ((recur (vertex)
               (when (not (member vertex seen :test (digraph-test digraph)))
                 (push vertex seen)
                 (funcall function vertex)
                 ;;; todo maybe use jpl queues here...
                 (appendf remaining (succ digraph vertex)))
               (when remaining
                 (recur (pop remaining)))))
      (when (contains-vertex-p digraph start-vertex)
        (recur start-vertex))))
  nil)

(defun handle-view-sync (time-view)

  ;; TODO: Execute dependency graph for each callback/task
  ;; - share code with analyzer - use submit-receive-graph fn from main
  ;; - controller callbacks need to add to a graph
  ;; - poss flatten callback tasks, use funcall here, callbacks place receive result as needed

  ;; input callbacks segregate by domain
  ;; output callbacks like cont anims
  ;; - option for recv-res
  ;;   - delay n events, or n=-1 for end of loop, n=0 for immediately after
  ;; - for example anims can run while processing input...what if one anim interferes with another? dep graph can resolve it
  ;; - for anims, need to be able to stop/pause/rewind/ffwd etc -> besides stop, others rely on undo system

  ;; nodal anims can be moved to shader, however have to pass in data

  ;; Loop
  ;; - controller will build dep graphs, enqueue task list
  ;;   - breadth-first search on graph to create levels, then execute levels
  ;;   - memoize nodes to speed up
  
  ;; Execute frame tasks
  ;; - Non-frame tasks would run in other thread - complicates things...
  ;; - optimistic loop - if > 16.7 ms: break (do on next frame)
  (loop
     :with counter := 0
     :for task := (sb-concurrency:dequeue *queue-input*)
     :while task
     :do (destructuring-bind (channel
			      fn
			      seq-key)
	     task
	   (if channel
	       (progn
		 ;; For now, assume same channel for all
		 ;; or add channel to list and receive-result for each channel
		 (submit-task channel
			      fn
			      seq-key)
		 (incf counter))
	       (funcall fn seq-key)))
     :finally (dotimes (i counter)
		(receive-result *channel*)))
  
  ;; Execute shm copy tasks
  ;; - Can be done in parallel assuming no overlapping operations...
  ;; - Can add detection code
  (loop
     :for counter := 0
     :for task := (sb-concurrency:dequeue *queue-view*)
     :while task
     :do (destructuring-bind (channel
			      name-shm
			      data
			      offset)
	     task
	   (incf counter)
	   (submit-task *channel*
			#'copy-data-to-shm
			name-shm
			data
			offset))
     :finally (dotimes (i counter)
		(receive-result *channel*)))

  ;; Is it faster to copy large contiguous area or numerous small segments?
  ;; Send message to view to copy shm
  ;; Use highest offset from copy shm queue...
  ;;
  ;; TODO:
  ;; digraph:count-vertices - checks hash-table-size
  ;; digraph:count-edges    - loops through vertices then edges
  ;; Reimplement cl-digaph with cl-tries
  (memcpy-shm-to-cache-flag* (list (list "nodes"
				       	 0
      				       	 (* +size-struct-instance+ (+ (digraph:count-vertices *digraph*)
				       				      (digraph:count-edges *digraph*))))
				   (list "projview"
				       	 0
      				       	 (* 4 16 2))))
      
  ;; Swap queues
  ;; If controller writes to queue sometime between loop and swap
  ;; it will be delayed by one frame
  ;; setf atomic? -> https://sourceforge.net/p/sbcl/mailman/message/14171520/

  (setf *queue-input* (if (eq *queue-input* *queue-front*)
  			  *queue-back*
  			  *queue-front*))

  t)
