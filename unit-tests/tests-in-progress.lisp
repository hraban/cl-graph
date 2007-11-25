(in-package cl-graph)

(defun foo ()
  (let ((graph (cl-graph:make-graph 'cl-graph:graph-container 
				    :vertex-test #'equal)))
    (cl-graph:add-vertex graph "a")
    (cl-graph:add-vertex graph "b")
    (cl-graph:add-vertex graph "c")
    (cl-graph:add-vertex graph "d")
    (cl-graph:add-vertex graph "e")
    (cl-graph:add-edge-between-vertexes graph "a" "b" :edge-type :directed)
    (cl-graph:add-edge-between-vertexes graph "b" "c" :edge-type :directed)
    (cl-graph:add-edge-between-vertexes graph "c" "a" :edge-type :directed)
    (cl-graph:add-edge-between-vertexes graph "d" "e" :edge-type :directed)
    graph))

(loop for component in 
     (cl-graph:find-connected-components (foo)) 
     for index from 1 do
     (format t "~&Component ~D (~d node~:p and ~d edge~:p)" 
	     index (vertex-count component) (edge-count component))
     (iterate-edges component (lambda (edge)
				(format t "~&  ~a to ~a" 
					(source-vertex edge) 
					(target-vertex edge))))
     (format t "~%"))


(defun mk-graph ()
    (let ((graph (cl-graph:make-graph 'cl-graph:graph-container
				    :vertex-test #'equal)))
    (cl-graph:add-vertex graph "a")
    (cl-graph:add-vertex graph "b")
    (cl-graph:add-vertex graph "c")
    (cl-graph:add-vertex graph "d")
    (cl-graph:add-vertex graph "e")
    (cl-graph:add-edge-between-vertexes graph "a" "b" :edge-type :directed)
    (cl-graph:add-edge-between-vertexes graph "b" "c" :edge-type :directed)
    (cl-graph:add-edge-between-vertexes graph "c" "a" :edge-type :directed)
    (cl-graph:add-edge-between-vertexes graph "d" "e" :edge-type :directed)
    graph))

(mk-graph)

(setf *g* (mk-graph))

(mapcar (lambda (v)
	  (list v (cl-graph:in-cycle-p *g* v)))
	(cl-graph:vertexes *g*))

(car (cl-graph:vertexes *g*))
