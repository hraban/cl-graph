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
    
