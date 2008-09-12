(in-package #:cl-graph-test)

(defun build-single-diamond-graph (style)
;;;;      /- c -\  
;;;; a - b       d - e 
;;;;      \- i -/    
  (let ((g (make-container 'graph-container)))
    (loop for (source . target) in '((a . b) 
				     (b . c) (b . i) (c . d) (i . d)
				     (d . e)) do
	 (add-edge-between-vertexes g source target :edge-type style))
    g))

(defun build-three-way-graph ()
  (let ((g (make-container 'graph-container)))
     (loop for (source . target) in '((a . b) 
				      (b . c) (b . d) (b . e) 
				      (c . f) (d . f) (e . f)
				      (f . g)) do
	  (add-edge-between-vertexes g source target))
     g))

(deftestsuite test-api (cl-graph-test)
  (g))

(addtest (test-api
	  :documentation "case 214")
  source-edges
  (let* ((g (build-single-diamond-graph :directed))
	 (b (find-vertex g 'b))
	 (target-edges (target-edges b))
	 (source-edges (source-edges b)))
    (ensure (every (lambda (edge)
		     (eq b (source-vertex edge)))
		   source-edges) :report "sources")
    (ensure (every (lambda (edge)
		     (eq b (target-vertex edge)))
		   target-edges) :report "targets")))
    
(addtest (test-api
	  :documentation "case 218")
  parents-of-child-vertexes
  (let* ((g (build-single-diamond-graph :directed))
	 (b (find-vertex g 'b))
	 (child-vertexes (child-vertexes b)))
    (ensure (every (lambda (vertex)
		     (member b (parent-vertexes vertex)))
		   child-vertexes) :report "children")))

(addtest (test-api
	  :documentation "case 218")
  children-of-parent-vertexes
  (let* ((g (build-single-diamond-graph :directed))
	 (b (find-vertex g 'b))
	 (parent-vertexes (parent-vertexes b)))
    (ensure (every (lambda (vertex)
		     (member b (child-vertexes vertex)))
		   parent-vertexes) :report "parents")))

(addtest (test-api
	  :documentation "case 218")
  parents-and-children=are-correct
  (let* ((g (build-single-diamond-graph :directed))
	 (b (find-vertex g 'b))
	 (child-vertexes (child-vertexes b))
	 (parent-vertexes (parent-vertexes b)))
    (ensure-same child-vertexes (list (find-vertex g 'c)
				      (find-vertex g 'i))
		 :test 'set-equal)
    (ensure-same parent-vertexes (list (find-vertex g 'a))
		 :test 'set-equal)))
