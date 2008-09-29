;; 2008-09-23 - these are the only bits that depend on moptilities

(in-package #:cl-graph)


;;; ---------------------------------------------------------------------------
;;; make-filtered-graph
;;; ---------------------------------------------------------------------------

(defmethod complete-links ((new-graph basic-graph) 
                           (old-graph basic-graph))
  ;; Copy links from old-graph ONLY for nodes already in new-graph
  (iterate-vertexes 
   new-graph
   (lambda (vertex)
     (let ((old-graph-vertex (find-vertex old-graph (value vertex))))
       (iterate-edges
        old-graph-vertex
        (lambda (old-edge)
          (let* ((old-other-vertex (other-vertex old-edge old-graph-vertex))
                 (new-other-vertex (find-vertex new-graph (value old-other-vertex) nil)))
            (when (and new-other-vertex
                       (< (vertex-id vertex) (vertex-id new-other-vertex)))
              (let* ((new-edge (mopu:copy-template old-edge)))
                (if (eq old-graph-vertex (vertex-1 old-edge))
                  (setf (slot-value new-edge 'vertex-1) vertex
                        (slot-value new-edge 'vertex-2) new-other-vertex)
                  (setf (slot-value new-edge 'vertex-2) vertex
                        (slot-value new-edge 'vertex-1) new-other-vertex))
                (add-edge new-graph new-edge))))))))))

#+Old
(defmethod complete-links ((new-graph basic-graph) 
                           (old-graph basic-graph))
  ;; Copy links from old-graph ONLY for nodes already in new-graph
  (iterate-vertexes 
   new-graph
   (lambda (vertex)
     (let ((old-graph-vertex (find-vertex old-graph (value vertex))))
       (iterate-edges
        old-graph-vertex
        (lambda (edge)
          (let* ((old-other-vertex (other-vertex edge old-graph-vertex))
                 (new-other-vertex (find-vertex new-graph (value old-other-vertex) nil))
                 (edge-type (if (directed-edge-p edge)
                              :directed :undirected)))
            (when new-other-vertex
              (if (and (directed-edge-p edge)
                       (eq old-graph-vertex (target-vertex edge)))
                (add-edge-between-vertexes new-graph new-other-vertex vertex
                                           :value (value edge)
                                           :edge-type edge-type)
                (add-edge-between-vertexes new-graph vertex new-other-vertex
                                           :value (value edge)
                                           :edge-type edge-type))))))))))

;;; ---------------------------------------------------------------------------

(defmethod make-filtered-graph ((old-graph basic-graph)
                                test-fn
                                &key
                                (graph-completion-method nil)
                                (depth nil)
				(new-graph 
				 (mopu:copy-template old-graph)))
  (ecase graph-completion-method
    ((nil 
      :complete-links)
     (iterate-vertexes old-graph
		       (lambda (vertex)
			 (when (funcall test-fn vertex)
			   (add-vertex new-graph (value vertex))))))
    ((:complete-closure-nodes-only 
      :complete-closure-with-links)
     (let* ((old-graph-vertexes  (collect-items old-graph :filter test-fn))
	    (closure-vertexes 
	     (get-transitive-closure old-graph-vertexes depth)))
       (dolist (vertex closure-vertexes)
	 (add-vertex new-graph (mopu:copy-template vertex))))))
  (ecase graph-completion-method
      ((nil :complete-closure-nodes-only) nil)
      ((:complete-links
        :complete-closure-with-links)
       (complete-links new-graph old-graph)))
  new-graph)

;;; ---------------------------------------------------------------------------

(defmethod subgraph-containing ((graph basic-graph) (vertex basic-vertex)
                                &rest args &key (depth nil) (new-graph nil))
  (declare (ignore depth new-graph))
  (apply #'make-filtered-graph
	 graph
	 #'(lambda (v)
	     (equal v vertex))
	 :graph-completion-method :complete-closure-with-links
	 args))

;;; ---------------------------------------------------------------------------
;;; for completeness 
;;; ---------------------------------------------------------------------------

(defmethod make-graph-from-vertexes ((vertex-list list))
  (bind ((edges-to-keep nil)
         (g (mopu:copy-template (graph (first vertex-list)))))
        
    (iterate-elements
     vertex-list
     (lambda (v)
       (add-vertex g (element v))
       (iterate-elements
        (edges v)
        (lambda (e)
          (when (and (member (vertex-1 e) vertex-list)
                     (member (vertex-2 e) vertex-list))
            (pushnew e edges-to-keep :test #'eq))))))
    
    (iterate-elements
     edges-to-keep
     (lambda (e)
       (bind ((v1 (source-vertex e))
              (v2 (target-vertex e)))
         ;;?? can we use copy here...
         (add-edge-between-vertexes
          g (element v1) (element v2)
          :edge-type (if (directed-edge-p e)
                       :directed
                       :undirected)
          :if-duplicate-do :force
          :edge-class (type-of e)
          :value (value e)
          :edge-id (edge-id e)
          :element (element e)
          :tag (tag e)
          :graph g
          :color (color e)))))
    g))
