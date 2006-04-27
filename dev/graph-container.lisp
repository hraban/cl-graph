
#| simple-header

$Id: graph-container.lisp,v 1.12 2005/07/20 20:39:09 moody Exp $

Author: Gary King

DISCUSSION

|#

(in-package metabang.graph)

;;; ---------------------------------------------------------------------------
;;; class defs
;;; ---------------------------------------------------------------------------

(defclass* graph-container (iteratable-container-mixin
                            non-associative-container-mixin
                            initial-contents-mixin
                            basic-graph
                            container-uses-nodes-mixin)
  ()
  (:default-initargs
    :vertex-class 'graph-container-vertex
    :directed-edge-class 'graph-container-directed-edge
    :undirected-edge-class 'graph-container-edge)
  (:export-p t)
  (:documentation "A graph container is essentially an adjacency list graph representation [?? The Bad name comes from it being implemented with containers... ugh]"))

;;; ---------------------------------------------------------------------------

(defclass* graph-container-edge (basic-edge)
  ((vertex-1 nil ir "`Vertex-1` is one of the two vertexes that an edge connects. In a directed-edge, `vertex-1` is also the `source-edge`.")
   (vertex-2 nil ir "`Vertex-2` is one of the two vertexes that an edge connects. In a directed edge, `vertex-2` is also the `target-vertex`."))
  (:export-slots vertex-1 vertex-2)
  (:export-p t)
  (:documentation "This is the root class for edges in graph-containers. It adds vertex-1 and vertex-2 slots."))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((object graph-container-edge) stream)
  (print-unreadable-object (object stream :type t) 
    (format stream "<~A ~A ~A>" (vertex-1 object) (vertex-2 object) 
            (value object))))

;;; ---------------------------------------------------------------------------

(defclass* weighted-edge (weighted-edge-mixin graph-container-edge)
  ()
  (:export-p t)
  (:documentation "A weighted edge is both a weighted-edge-mixin and a graph-container-edge."))

;;; ---------------------------------------------------------------------------

(defclass* graph-container-vertex (basic-vertex)
  ((vertex-edges nil r))
  (:export-p t)
  (:default-initargs 
    :vertex-edges-container-class 'vector-container)
  (:documentation "A graph container vertex keeps track of its edges in the the vertex-edges slot. The storage for this defaults to a vector-container but can be changed using the vertex-edges-container-class initarg."))

;;; ---------------------------------------------------------------------------

(defmethod make-vertex-edges-container ((vertex graph-container-vertex) 
                                        container-class &rest args)
  (apply #'make-container container-class args))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object graph-container-vertex) &key
                                       vertex-edges-container-class)
  (setf (slot-value object 'vertex-edges)
        (make-vertex-edges-container object vertex-edges-container-class)))

;;; ---------------------------------------------------------------------------

(defmethod make-vertex-container ((graph graph-container) initial-size) 
  (make-container 'simple-associative-container
                  :initial-size initial-size
                  :test (vertex-test graph)))

;;; ---------------------------------------------------------------------------

(defmethod make-edge-container ((graph graph-container) initial-size) 
  (make-container 'vector-container :initial-size initial-size
                  :fill-pointer 0))


;;; ---------------------------------------------------------------------------
;;; graph-container-directed-edge
;;; ---------------------------------------------------------------------------

(defclass* graph-container-directed-edge (directed-edge-mixin 
                                          graph-container-edge)
  ()
  (:export-p t)
  (:documentation "A graph-container-directed-edge is both a directed-edge-mixin and a graph-container-edge."))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object graph-container-directed-edge)
                                       &key source-vertex target-vertex)
  (when (and source-vertex (vertex-1 object))
    (error "Specify source-vertex or vertex-1, but not both"))
  (when (and target-vertex (vertex-2 object))
    (error "Specify target-vertex or vertex-2, but not both"))
  (when source-vertex
    (setf (slot-value object 'vertex-1) source-vertex))
  (when target-vertex
    (setf (slot-value object 'vertex-2) target-vertex)))

;;; ---------------------------------------------------------------------------
;;; vertex-1 is defined to be the source vertex of an undirected edge
;;; ---------------------------------------------------------------------------

(defmethod source-vertex ((edge graph-container-edge))
  (vertex-1 edge))

;;; ---------------------------------------------------------------------------
;;; vertex-2 is defined to be the target vertex of an undirected edge
;;; ---------------------------------------------------------------------------

(defmethod target-vertex ((edge graph-container-edge))
  (vertex-2 edge))

;;; ---------------------------------------------------------------------------

(defmethod other-vertex ((edge graph-container-edge) 
                         (v graph-container-vertex))
  (cond ((eq v (vertex-1 edge))
         (values (vertex-2 edge)))
        
        ((eq v (vertex-2 edge))
         (values (vertex-1 edge)))
        
        (t (error "Vertex ~A not part of Edge ~A" v edge))))

;;; ---------------------------------------------------------------------------

(defmethod other-vertex ((edge graph-container-edge) 
                         (value t))
  (other-vertex edge (find-vertex edge value)))

;;; ---------------------------------------------------------------------------

(defmethod add-edge ((graph graph-container) (edge graph-container-edge)
                     &key force-new?)
  (declare (ignore force-new?))
  
  (bind ((vertex-1 (vertex-1 edge))
         (vertex-2 (vertex-2 edge)))
        
    (cond ((eq vertex-1 vertex-2)
           (add-edge-to-vertex edge vertex-1))
          (t
           (add-edge-to-vertex edge vertex-1)
           (add-edge-to-vertex edge vertex-2))))
  edge)

;;; ---------------------------------------------------------------------------

(defmethod add-edge-to-vertex :around ((edge graph-container-edge) 
                                       (vertex graph-container-vertex))
  (insert-item (vertex-edges vertex) edge))

;;; ---------------------------------------------------------------------------

(defmethod make-node-for-container ((graph graph-container) (node t) &key)
  (make-vertex-for-graph graph :element node))

;;; ---------------------------------------------------------------------------

(defmethod find-edge-between-vertexes ((graph graph-container) 
                                       (vertex-1 graph-container-vertex) 
                                       (vertex-2 graph-container-vertex)
                                       &key error-if-not-found?)
  (declare (ignore error-if-not-found?))
  (search-for-match (vertex-edges vertex-1)
                    (lambda (edge)
                      (eq vertex-2 (other-vertex edge vertex-1)))))

;;; ---------------------------------------------------------------------------

(defmethod find-edge-between-vertexes-if ((graph graph-container) 
                                          (vertex-1 graph-container-vertex) 
                                          (vertex-2 graph-container-vertex)
                                          fn
                                          &key error-if-not-found?)
  (declare (ignore error-if-not-found?))
  (search-for-match (vertex-edges vertex-1)
                    (lambda (edge)
                      (and (eq vertex-2 (other-vertex edge vertex-1))
                           (funcall fn edge)))))

;;; ---------------------------------------------------------------------------

(defmethod find-edge-between-vertexes-if ((graph graph-container) 
                                          (value-1 t) 
                                          (value-2 t)
                                          fn
                                          &key error-if-not-found?)
  (bind ((v1 (find-vertex graph value-1 error-if-not-found?))
         (v2 (find-vertex graph value-2 error-if-not-found?)))
    (find-edge-between-vertexes-if 
     graph v1 v2 fn 
     :error-if-not-found? error-if-not-found?)))

;;; ---------------------------------------------------------------------------

(defmethod find-edge ((graph graph-container) (edge graph-container-edge)
                      &optional error-if-not-found?)
  (find-edge-between-vertexes
   graph (vertex-1 edge) (vertex-2 edge)
   :error-if-not-found? error-if-not-found?))

;;; ---------------------------------------------------------------------------

(defmethod delete-edge ((graph graph-container) (edge graph-container-edge))
  (delete-item (vertex-edges (vertex-1 edge)) edge)
  (delete-item (vertex-edges (vertex-2 edge)) edge)
  edge)

;;; ---------------------------------------------------------------------------

(defmethod iterate-edges ((graph graph-container) fn)
  (iterate-elements (graph-edges graph) fn))

;;; ---------------------------------------------------------------------------

(defmethod iterate-edges ((vertex graph-container-vertex) fn)
  (iterate-elements (vertex-edges vertex) fn))

;;; ---------------------------------------------------------------------------

(defmethod iterate-source-edges ((vertex graph-container-vertex) fn)
  (iterate-elements (vertex-edges vertex)
                    (lambda (edge)
                      (when (or (undirected-edge-p edge)
                                (eq vertex (target-vertex edge)))
                        (funcall fn edge)))))

;;; ---------------------------------------------------------------------------

(defmethod iterate-target-edges ((vertex graph-container-vertex) fn)
  (iterate-elements (vertex-edges vertex)
                    (lambda (edge)
                      (when (or (undirected-edge-p edge)
                                (eq vertex (source-vertex edge)))
                        (funcall fn edge)))))

;;; ---------------------------------------------------------------------------

(defmethod iterate-children ((vertex graph-container-vertex) fn)
  (iterate-target-edges vertex
                        (lambda (edge)
                          (funcall fn (other-vertex edge vertex)))))

;;; ---------------------------------------------------------------------------

(defmethod iterate-parents ((vertex graph-container-vertex) fn)
  (iterate-source-edges vertex
                        (lambda (edge)
                          (funcall fn (other-vertex edge vertex)))))

;;; ---------------------------------------------------------------------------

(defmethod iterate-neighbors ((vertex graph-container-vertex) fn)
  (iterate-edges vertex
                 (lambda (edge)
                   (funcall fn (other-vertex edge vertex)))))

;;; ---------------------------------------------------------------------------

(defmethod vertexes ((edge graph-container-edge))
  (collect-using #'iterate-vertexes nil edge))

;;; ---------------------------------------------------------------------------

(defmethod has-children-p ((vertex graph-container-vertex))
  (iterate-target-edges vertex
                        (lambda (edge)
                          (declare (ignore edge))
                          (return-from has-children-p t)))
  (values nil))

;;; ---------------------------------------------------------------------------

(defmethod has-parent-p ((vertex graph-container-vertex))
  (iterate-source-edges vertex
                        (lambda (edge)
                          (declare (ignore edge))
                          (return-from has-parent-p t)))
  (values nil))

;;; ---------------------------------------------------------------------------

(defmethod vertices-share-edge-p ((vertex-1 graph-container-vertex) 
                                  (vertex-2 graph-container-vertex))
  (iterate-target-edges vertex-1
                        (lambda (e)
                          (when (or (eq (target-vertex e) vertex-2)
                                    (eq (source-vertex e) vertex-2))
                            (return-from vertices-share-edge-p t))))
  
  (iterate-source-edges vertex-1
                        (lambda (e)
                          (when (or (eq (target-vertex e) vertex-2)
                                    (eq (source-vertex e) vertex-2))
                            (return-from vertices-share-edge-p t))))
  
  (values nil))

;;; ---------------------------------------------------------------------------

(defmethod edge-count ((graph graph-container))
  (size (graph-edges graph)))

