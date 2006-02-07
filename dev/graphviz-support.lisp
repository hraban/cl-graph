;;;-*- Mode: Lisp; Package: metabang.graph -*-

#| simple-header

$Id: graphviz-support.lisp,v 1.7 2005/06/21 20:51:51 moody Exp $

Copyright 1992 - 2005 Experimental Knowledge Systems Lab, 
University of Massachusetts Amherst MA, 01003-4610
Professor Paul Cohen, Director

Author: Gary King

DISCUSSION

A color value can be a huesaturation-
brightness triple (three floating point numbers between 0 and 1, separated
by commas); one of the colors names listed in Appendix G (borrowed from
some version of the X window system); or a red-green-blue (RGB) triple4 (three
hexadecimal number between 00 and FF, preceded by the character Õ#Õ). Thus,
the values "orchid", "0.8396,0.4862,0.8549" and #DA70D6 are three
ways to specify the same color.

|#
(in-package metabang.graph)

;;; ---------------------------------------------------------------------------
;
; This outputs the graph to string in accordance with the DOT file format.  
; For more information about DOT file format, search the web for "DOTTY" and 
; "GRAPHVIZ".
;
(defmethod graph->dot ((g basic-graph) (stream stream)
                       &key 
                       (graph-formatter 'graph->dot-properties)
                       (vertex-key 'vertex-id)
                       (vertex-labeler nil)
                       (vertex-formatter 'vertex->dot)
                       (edge-key nil)
                       (edge-labeler 'princ) 
                       (edge-formatter 'edge->dot))
  (format stream "~A G {~%graph " (if (contains-undirected-edge-p g) "graph" "digraph"))
  (format stream "[")
  (funcall graph-formatter g stream)
  (format stream "];")
  (terpri stream)
  
  ;; vertex formatting
  (iterate-vertexes 
   g
   (lambda (v)
     (terpri stream)
     (let ((key (if vertex-key (funcall vertex-key v) v)))
       (princ key stream)
       (princ " [" stream)
       (when vertex-labeler
         (princ "label=\"" stream)
         (funcall vertex-labeler v stream)
         (princ "\", " stream))
       (funcall vertex-formatter v stream)
       (princ "]" stream))))
  
  (let ((directed-edge-connector (if (contains-undirected-edge-p g) "--" "->"))
        (directed-edge-tag (when (and (contains-undirected-edge-p g)
                                      (contains-directed-edge-p g))
                             "dir=forward, ")))
    (flet ((format-edge (e connector from to directed?)
             (terpri stream)
             (princ (funcall vertex-key from) stream)
             (princ connector stream)
             (princ (funcall vertex-key to) stream) 
             (princ " [" stream)
             (when (and directed? directed-edge-tag)
               (princ directed-edge-tag stream))
             (when edge-key
               (princ "label=\"" stream)
               (funcall edge-labeler e stream)
               (princ "\"," stream))
             (funcall edge-formatter e stream)
             (princ "]" stream)))
      ;; directed edges
      (iterate-vertexes 
       g
       (lambda (v)
         (iterate-target-edges
          v
          (lambda (e) 
            (when (directed-edge-p e)
              (format-edge e directed-edge-connector 
                           (source-vertex e) (target-vertex e) t))))))
      
      ;; undirected edges
      (let ((edges (make-container 'simple-associative-container)))
        (iterate-vertexes 
         g
         (lambda (v)
           (iterate-edges
            v
            (lambda (e)
              (when (and (undirected-edge-p e)
                         (not (item-at-1 edges e)))
                (setf (item-at-1 edges e) t)
                (format-edge e "--" (vertex-1 e) (vertex-2 e) nil)))))))))
  
  (terpri stream)
  (princ "}" stream)
  
  (values g))


#+Test
(let ((g (make-container 'graph-container :default-edge-type :undirected)))
  (loop for (a b) in '((a b) (b c) (b d) (d e) (e f) (d f)) do
        (add-edge-between-vertexes g a b))
  (graph->dot g nil))

#+Test
"graph G {
E []
C []
B []
A []
D []
F []
D--E []
E--F []
B--C []
A--B []
B--D []
D--F []
}"

#+Test
(let ((g (make-container 'graph-container :default-edge-type :directed)))
  (loop for (a b) in '((a b) (b c) (b d) (d e) (e f) (d f)) do
        (add-edge-between-vertexes g a b))
  (graph->dot g nil))

#+Test
"digraph G {
E []
C []
B []
A []
D []
F []
E->F []
B->C []
B->D []
A->B []
D->E []
D->F []
}"

#+Test
(let ((g (make-container 'graph-container)))
  (loop for (a b) in '((d e) (e f) (d f)) do
        (add-edge-between-vertexes g a b :edge-type :directed))
  (loop for (a b) in '((a b) (b c) (b d)) do
        (add-edge-between-vertexes g a b :edge-type :undirected))
  (graph->dot g nil))

#+Test
"graph G {
E []
C []
B []
A []
D []
F []
E--F [dir=forward, ]
D--E [dir=forward, ]
D--F [dir=forward, ]
B--C []
A--B []
B--D []
}"

;;; ---------------------------------------------------------------------------

(defmethod graph->dot ((g basic-graph) (stream (eql nil))
                       &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (let ((out (make-string-output-stream)))
    (apply #'graph->dot g out args)
    (get-output-stream-string out)))

;;; ---------------------------------------------------------------------------

(defmethod graph->dot ((g basic-graph) (stream (eql t))
                       &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (apply #'graph->dot g *standard-output* args))

;;; ---------------------------------------------------------------------------

(defmethod graph->dot ((g basic-graph) (stream string)
                       &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (with-open-file (out stream :direction :output :if-exists :supersede)
    (apply #'graph->dot g out args)))

;;; ---------------------------------------------------------------------------

(defmethod graph->dot ((g basic-graph) (stream pathname)
                       &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (apply #'graph->dot g (namestring stream) args))

;;; ---------------------------------------------------------------------------

(defmethod graph->dot-properties ((g t) (stream t))
  (values))

;;; ---------------------------------------------------------------------------

(defmethod vertex->dot ((v basic-vertex) (stream stream))
  (values))

;;; ---------------------------------------------------------------------------

(defmethod edge->dot ((v basic-edge) (stream stream))
  (values))

;;; ---------------------------------------------------------------------------
;;; dot->graph
;;; ---------------------------------------------------------------------------

#|
(defmethod dot->graph ((dot-stream stream)
                       &key)
  )

;;; ---------------------------------------------------------------------------

(defmethod dot->graph ((dot-stream string)
                       &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (with-open-file (out stream :direction :output :if-exists :supersede)
    (apply #'dot->graph g out args)))

;;; ---------------------------------------------------------------------------

(defmethod dot->graph ((dot-stream pathname)
                       &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (with-open-file (out stream :direction :output :if-exists :supersede)
    (apply #'dot->graph g out args))
  (apply #'dot->graph g (namestring stream) args))

|#