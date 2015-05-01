;;;-*- Mode: Lisp; Package: metabang.graph -*-

#| simple-header

$Id: graphviz-support-optional.lisp,v 1.0 2005/06/21 20:51:51 moody Exp $

Author: Attila Lendvai

DISCUSSION

This file contains the stuff that depends on hu.dwim.graphviz and is only
loaded when hu.dwim.graphviz is available.

|#

(in-package #:metabang.graph)

;; TODO these are hacks to be removed later,
;; the functionality should be provided by graph itself
(defgeneric find-vertex-by-id (graph id)
  (:method (graph (id integer))
    (search-for-vertex graph id :key 'vertex-id))

  (:method (graph (id string))
    (find-vertex-by-id graph (parse-integer id))))

(defgeneric layout-graph-with-graphviz (graph &key algorithm)
  (:method ((graph dot-graph) &key (algorithm nil algorithm-provided-p))
    (let* ((dot (with-output-to-string (out) (graph->dot graph out)))
           (args (list dot
                       :graph-visitor
                       (lambda (dot-graph)
                         (setf (dot-attribute-value :bb graph)
                               (hu.dwim.graphviz:graph-bounding-box dot-graph)))

                       :node-visitor
                       (lambda (node)
                         (bind ((pos (hu.dwim.graphviz:node-coordinate node))
                                ((width height) (hu.dwim.graphviz:node-size node)))
                           ;;(format t "Node ~a: ~a; ~a, ~a~%"
                           ;;        (hu.dwim.graphviz:node-name node)
                           ;;        pos
                           ;;        width height)
                           ;; TODO search-for-vertex is sloooow, use a hashtable or
                           ;; introduce an graph-find-element-by-id-mixin, or similar
                           (let ((vertex (find-vertex-by-id graph (hu.dwim.graphviz:node-name node))))
                             (setf (dot-attribute-value :pos vertex) pos
                                   (dot-attribute-value :width vertex) width
                                   (dot-attribute-value :height vertex) height))))

                       :edge-visitor
                       (lambda (edge)
                         (bind (((from to) (hu.dwim.graphviz:edge-between edge))
                                (label (hu.dwim.graphviz::edge-label edge))
                                (pos (when label (hu.dwim.graphviz::label-coordinate label))))
                           ;;(format t "Edge: ~a - ~a~%"
                           ;;        (hu.dwim.graphviz:node-name from)
                           ;;        (hu.dwim.graphviz:node-name to))
                           (let* ((from-vertex (find-vertex-by-id graph (hu.dwim.graphviz:node-name from)))
                                  (to-vertex (find-vertex-by-id graph (hu.dwim.graphviz:node-name to)))
                                  (real-edge (find-edge-between-vertexes graph from-vertex to-vertex))
                                  (bezier-points '()))
                             (hu.dwim.graphviz:iterate-edge-beziers
                              edge
                              (lambda (bezier)
                                ;;(format t "  Bezier: ~a~%"
                                ;;        (hu.dwim.graphviz:bezier-points bezier))
                                (dolist (el (hu.dwim.graphviz:bezier-points bezier))
                                  (push el bezier-points))))
                             (setf (dot-attribute-value :pos real-edge) (nreverse bezier-points)
                                   (dot-attribute-value :lp real-edge) pos)))))))
      (when algorithm-provided-p
        (nconc args (list :algorithm algorithm)))
      (apply 'hu.dwim.graphviz:layout-dot-format args))
    graph))
