(in-package #:cl-graph)

;;; make a simple graph
(let ((g (make-container 'graph-container))) 
  (loop for v in '(a b c d e) do
        (add-vertex g v))
  (loop for (v1 . v2) in '((a . b) (a . c) (b . d) (c . e)) do
        (add-edge-between-vertexes g v1 v2))
  g)

;;; make a directed graph
;; adding the vertexes up front not really necessary
(let ((g (make-container 'graph-container :default-edge-type :directed))) 
  (loop for (v1 . v2) in '((a . b) (a . c) (b . d) (c . e)) do
        (add-edge-between-vertexes g v1 v2))
  g)


;;; make a graph, find some things
(let ((g (make-container 'graph-container)))
    (loop for (src dst) in '((a b) (a c) (c d) (a d) (d e) (e f) (b f)) do
          (add-edge-between-vertexes g src dst))
    
    (print (find-vertex g 'a))
    (print (find-vertex g 'q nil))
    (print (find-edge-between-vertexes g 'a 'b))
    (print (find-edge-between-vertexes g 'a 'f :error-if-not-found? nil))
    
    (format t "~%Neighbors of vertex A:")
    (iterate-neighbors (find-vertex g 'a) #'print))