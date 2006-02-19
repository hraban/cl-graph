;;;-*- Mode: Lisp; Package: metabang.graph -*-

#| simple-header

$Id: graphviz-support-optional.lisp,v 1.0 2005/06/21 20:51:51 moody Exp $

Author: Attila Lendvai

DISCUSSION

This file contains the stuff that depends on cl-graphviz and is only
loaded when cl-graphviz is available.

|#

(in-package metabang.graph)

;; TODO these are hacks to be removed later,
;; the functionality should be provided by graph itself
(defmethod find-vertex-by-id (g (id integer))
  (search-for-vertex g id :key 'vertex-id))
(defmethod find-vertex-by-id (g (id string))
  (find-vertex-by-id g (parse-integer id)))

;;; ---------------------------------------------------------------------------
(defmethod layout-graph-with-graphviz ((g dot-graph)
                                       &key 
                                       (algorithm nil algorithm-provided-p))
  (let* ((dot (with-output-to-string (out) (graph->dot g out)))
         (args (list dot
                     :graph-visitor
                     (lambda (dot-graph)
                       (setf (dot-attribute :bb g)
                             (graphviz:graph-bounding-box dot-graph)))

                     :node-visitor
                     (lambda (node)
                       (bind ((pos (graphviz:node-coordinate node))
                              ((width height) (graphviz:node-size node)))
                         ;;(format t "Node ~a: ~a; ~a, ~a~%"
                         ;;        (graphviz:node-name node)
                         ;;        pos
                         ;;        width height)
                         ;; TODO search-for-vertex is sloooow, use a hashtable or
                         ;; introduce an graph-find-element-by-id-mixin, or similar
                         (let ((vertex (find-vertex-by-id g (graphviz:node-name node))))
                           (setf (dot-attribute :pos vertex) pos)
                           (setf (dot-attribute :width vertex) width)
                           (setf (dot-attribute :height vertex) height))))
                     
                     :edge-visitor
                     (lambda (edge)
                       (bind (((from to) (graphviz:edge-between edge)))
                         ;;(format t "Edge: ~a - ~a~%"
                         ;;        (graphviz:node-name from)
                         ;;        (graphviz:node-name to))
                         (let* ((from-vertex (find-vertex-by-id g (graphviz:node-name from)))
                                (to-vertex (find-vertex-by-id g (graphviz:node-name to)))
                                (real-edge (find-edge-between-vertexes g from-vertex to-vertex))
                                (bezier-points '()))
                           (graphviz:edge-iterate-beziers
                            edge
                            (lambda (bezier)
                              ;;(format t "  Bezier: ~a~%"
                              ;;        (graphviz:bezier-points bezier))
                              (dolist (el (graphviz:bezier-points bezier))
                                (push el bezier-points))))
                           (setf (dot-attribute :pos real-edge) (nreverse bezier-points))))))))
    (when algorithm-provided-p
      (nconc args (list :algorithm algorithm)))
    (apply 'graphviz:layout-dot-format args))
  g)


