(in-package :protoform.model)

(defclass directed-acyclic-graph ()
  ((vertices
    :documentation "hash table"
    :initform ()
    :accessor vertices)))

(defclass vertex ()
  ((data
    :documentation "data"
    :initform nil
    :accessor data)
   (edges
    :documentation "edges"
    :initform ()
    :accessor edges)))

;; Terminology:
;; ancestors/descendants
;; predecessors/sucessors
;; parent/child

; Vertex Functions

(defun add-vertex (graph data)
  (let ((vertex (make-instance 'vertex)))
    (setf (data vertex) data)
    (push vertex graph)
    vertex))

(defun remove-vertex (graph a b)
  ;; Remove B from A
  (delete a (vertices graph)))

; Edge Functions
(defun add-edge (graph a b)
  ;; Add B to A's edges
  (push b (edges a)))

(defun remove-edge (graph a b)
  ; Remove B from A
  (delete b (edges a)))


(defun add-list (graph branch root)
  ;; Recursive

  ;; Add vertex and edge for first item
  ;; {.}-{a}-{.}-{a}
  ;;          |
  ;;         {a}
  (let ((pred (add-vertex graph (first branch))))
    ;; Add edge to root
    (add-edge graph root pred)
    ;; Iterate if there are more things
    (when (cdr branch)
      (iter (for item in (cdr branch))
        ;; If list, call this function, otherwise add atom
        ;; For both set the previous vertex
        (cond ((listp item)
               (setf pred (add-list graph item pred)))      ; Set prev vertex
              (t ;atom
               (let ((vertex (add-vertex graph item)))  ; Create vertex
                  (add-edge graph pred item)                ; Connect back to prev vertex
                  (setf pred vertex))))))                   ; Set prev vertex
    pred)) ; Return root
