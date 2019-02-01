(in-package :protoform.model)

(defun add-node-vcs ()
  (let ((node (init-node-msdf (get-origin-from-node-pos *node-ptr-vcs*)
			      *scale-node*
			      (pop *stack-i-nodes*)
			      #\*)))
    
    (insert-vertex node *digraph-vcs* *vertices-vcs*)
    (spatial-trees:insert node *r-tree*)
    
    (insert-node node
      		 *node-ptr-vcs*
		 :out
		 *digraph-vcs*
		 *edges-vcs*)
    
    (advance-node-of-node *node-ptr-vcs*
			  node
  			  1.0)
    
    (enqueue-node *node-ptr-vcs*)
    (enqueue-node node)))  
