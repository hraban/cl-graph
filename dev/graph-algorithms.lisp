(in-package #:metabang.graph)

;;; ---------------------------------------------------------------------------
;;;
;;; ---------------------------------------------------------------------------

(defstruct (vertex-datum (:conc-name node-) (:type list))
  (color nil)
  (depth most-positive-fixnum)
  (parent nil))

;;; ---------------------------------------------------------------------------

(defmethod initialize-vertex-data ((graph basic-graph))
  (let ((vertex-data (make-container 'simple-associative-container)))
    (iterate-vertexes graph (lambda (v) 
                              (setf (item-at vertex-data v) 
                                    (make-vertex-datum :color :white))))
    (values vertex-data)))
  
;;; ---------------------------------------------------------------------------
;;; breadth-first-search by GWK
;;; ---------------------------------------------------------------------------

(defmethod breadth-first-visitor ((graph basic-graph) (source t) fn)
  (breadth-first-visitor graph (find-vertex graph source) fn))

;;; ---------------------------------------------------------------------------

(defmethod breadth-first-visitor ((graph basic-graph) (source basic-vertex) fn)
  ;; initialize
  (let ((vertex-data (initialize-vertex-data graph))
        (queue (make-container 'basic-queue)))
    
    (let ((source-datum (item-at vertex-data source)))
      (setf (node-color source-datum) :grey
            (node-depth source-datum) 0)
      (enqueue queue source)
      
      (loop until (empty-p queue) do
            (let* ((current-vertex (first-item queue))
                   (current (item-at vertex-data current-vertex)))
              ;(format t "~%~A:" current-vertex)
              (iterate-children current-vertex
                                (lambda (child-vertex)
                                  ;(format t "~A " child-vertex)
                                  (let ((child (item-at vertex-data child-vertex)))
                                    (when (eq (node-color child) :white)
                                      (setf (node-color child) :grey
                                            (node-depth child) (1+ (node-depth current))
                                            (node-parent child) current-vertex)
                                      (enqueue queue child-vertex)))))
              
              (dequeue queue)
              (setf (node-color current) :black)
              (funcall fn current-vertex)))
      
      vertex-data)))

;;; ---------------------------------------------------------------------------

(defmethod breadth-first-search-graph ((graph basic-graph) (source t))
  (breadth-first-search-graph graph (find-vertex graph source)))

;;; ---------------------------------------------------------------------------

(defmethod breadth-first-search-graph ((graph basic-graph) (source basic-vertex))
  ;; initialize
  (let ((vertex-data (initialize-vertex-data graph))
        (queue (make-container 'basic-queue)))
    
    (let ((source-datum (item-at vertex-data source)))
      (setf (node-color source-datum) :grey
            (node-depth source-datum) 0)
      (enqueue queue source)
      
      (loop until (empty-p queue) do
            (let* ((current-vertex (first-item queue))
                   (current (item-at vertex-data current-vertex)))
              ;(format t "~%~A:" current-vertex)
              (iterate-children current-vertex
                                (lambda (child-vertex)
                                  ;(format t "~A " child-vertex)
                                  (let ((child (item-at vertex-data child-vertex)))
                                    (when (eq (node-color child) :white)
                                      (setf (node-color child) :grey
                                            (node-depth child) (1+ (node-depth current))
                                            (node-parent child) current-vertex)
                                      (enqueue queue child-vertex)))))
              
              (dequeue queue)
              (setf (node-color current) :black)))
      
      vertex-data)))
    
;;; ---------------------------------------------------------------------------
;;; single-source-shortest-paths - gwk
;;; ---------------------------------------------------------------------------

#+NotYet
(defmethod single-source-shortest-paths ((graph basic-graph))
  (let ((vertex-data (initialize-vertex-data graph))
        (queue (make-container 'priority-queue-on-container 'binary-search-tree)))
    (let ((source-datum (item-at vertex-data source)))
      (setf (node-depth source-datum) 0))
    ))

;;; ---------------------------------------------------------------------------
;;; connected-components - gwk
;;; ---------------------------------------------------------------------------

(defmethod connected-components ((graph basic-graph))
  (let ((union (make-container 'union-find-container)))
    (iterate-vertexes
     graph
     (lambda (v) (insert-item union v)))
    (iterate-edges 
     graph 
     (lambda (e) 
       (let ((node-1 (representative-node union (vertex-1 e)))
             (node-2 (representative-node union (vertex-2 e))))
         (unless (eq (find-set node-1) (find-set node-2))
           (graft-nodes node-1 node-2)))))
    (iterate-elements union 'find-set)
    union))

;;; ---------------------------------------------------------------------------

(defmethod connected-component-count ((graph basic-graph))
  ;;?? Gary King 2005-11-28: Super ugh
  (size 
   (remove-duplicates
    (collect-elements 
     (connected-components graph)
     :transform #'parent)))
  
  #+Fails
  ;;?? Gary King 2005-11-28: fails on big graphs? iterator design
  ;;?? Gary King 2005-11-28: ideally we don't want to cons up the list at all
  (size 
   (collect-elements
    (make-iterator (connected-components graph) :unique t :transform #'parent))))

(defmethod find-connected-components ((graph basic-graph))
  (collect-elements
   (make-iterator (connected-components graph) :unique t :transform #'parent)
   :transform 
   (lambda (component)
     (subgraph-containing graph (element component) 
                          :depth most-positive-fixnum))))

#+Alternate
(defmethod find-connected-components ((graph basic-graph))
  (let ((result nil)
        (found-elements (make-container 'simple-associative-container)))
    (iterate-elements
     (connected-components graph)
     (lambda (component)
       (let ((element (element (parent component))))
         (unless (item-at found-elements element)
           (setf (item-at found-elements element) t)
           
           (push (subgraph-containing graph (element component) 
                                      most-positive-fixnum)
                 result)))))
    
    result))


         
;;; ---------------------------------------------------------------------------
;;; minimum-spanning-tree based on kruskal's algorithm detailed in clrs2 -jjm
;;; ---------------------------------------------------------------------------

(defmethod mst-find-set ((vertex basic-vertex))
  #+ignore
  (unless (previous-node vertex)
    (return-from mst-find-set nil))
  (unless (eq vertex (previous-node vertex))
    (setf (previous-node vertex) (mst-find-set (previous-node vertex))))
  (previous-node vertex))

;;; ---------------------------------------------------------------------------

(defmethod mst-make-set ((vertex basic-vertex))
  (setf (previous-node vertex) vertex
        (rank vertex) 0))

;;; ---------------------------------------------------------------------------

(defmethod mst-tree-union ((v1 basic-vertex) (v2 basic-vertex))
  (mst-link (mst-find-set v1) (mst-find-set v2)))

;;; ---------------------------------------------------------------------------

(defmethod mst-link ((v1 basic-vertex) (v2 basic-vertex))
  (cond ((> (rank v1) (rank v2))
         (setf (previous-node v2) v1))
        (t (setf (previous-node v1) v2)
           (when (= (rank v1) (rank v2))
             (incf (rank v2))))))

;;; ---------------------------------------------------------------------------
;;; jjm's implementation of mst depends on this
;;; todo - figure out some what to add and edge we create to a graph rather
;;; than always using add-edge-between-vertexes interface
;;; ---------------------------------------------------------------------------

(defmethod add-edges-to-graph ((graph basic-graph) (edges list) 
                               &key (if-duplicate-do :ignore))
  (iterate-elements
   edges
   (lambda (edge)
     (bind ((v1 (element (source-vertex edge)))
            (v2 (element (target-vertex edge))))
       (add-edge-between-vertexes
        graph v1 v2 :edge-class (type-of edge)
        :edge-type (if (directed-edge-p edge)
                     :directed
                     :undirected)
        :value (value edge)
        :edge-id (edge-id edge)
        :element (element edge)
        :tag (tag edge)
        :graph graph
        :color (color edge)
        :if-duplicate-do if-duplicate-do))))
  graph)

;;; ---------------------------------------------------------------------------

(defmethod edge-lessp-by-weight ((e1 basic-edge) (e2 basic-edge))
  (< (weight e1) (weight e2)))

;;; ---------------------------------------------------------------------------
;;; minumum spanning tree
;;; ---------------------------------------------------------------------------


(defmethod minimum-spanning-tree ((graph basic-graph) 
                                  &key
                                  (edge-sorter #'edge-lessp-by-weight))
  (bind ((result nil))
    (iterate-vertexes 
     graph
     (lambda (v)
       (mst-make-set v)))
    
    (loop for edge in (sort (edges graph) edge-sorter) do
          (bind ((v1 (source-vertex edge))
                 (v2 (target-vertex edge)))
            
            (unless (eq (mst-find-set v1)
                        (mst-find-set v2))
              (push edge result)
              (mst-tree-union v1 v2)))
          finally
          (return
           (cond ((= (length result) (- (length (vertexes graph)) 1))
                  (values t result))
                 (t (values nil result)))))))

;;; ---------------------------------------------------------------------------

#+ignore ;;; shoot
(defmethod minimum-spanning-tree ((vertex-list list) 
                                  &key
                                  (edge-sorter #'edge-lessp-by-weight))
  (bind ((result nil)
         (v-edges (remove-duplicates 
                   (flatten (mapcar #'edges vertex-list)) :test #'eq)))
    
    (iterate-container
     vertex-list
     (lambda (v)
       (mst-make-set v)))    
    
    (loop for edge in (sort v-edges edge-sorter) do
          (bind ((v1 (source-vertex edge))
                 (v2 (target-vertex edge))
                 (v1-set (mst-find-set v1))
                 (v2-set (mst-find-set v2)))

            (when (or (not v1-set)
                           (not v2-set))
              (return-from minimum-spanning-tree nil))
            
            
            (unless (eq (mst-find-set v1)
                        (mst-find-set v2))
              (push edge result)
              (mst-tree-union v1 v2)))
          finally
          (return
           (cond ((= (length result) (- (length vertex-list) 1))
                  (values t result))
                 (t (values nil result)))))))

;;; ---------------------------------------------------------------------------
;;; uses mst to determine if the graph is connected
;;; ---------------------------------------------------------------------------

(defmethod connected-graph-p ((graph basic-graph) &key 
                              (edge-sorter 'edge-lessp-by-weight))
  (minimum-spanning-tree graph :edge-sorter edge-sorter))

  
;;; ---------------------------------------------------------------------------

#+test
(bind ((g (make-container 'graph-container)))
  (add-edge-between-vertexes g :v :y :edge-type :directed)
  (add-edge-between-vertexes g :u :x :edge-type :directed)
  (add-edge-between-vertexes g :x :v :edge-type :directed)
  (add-edge-between-vertexes g :u :v :edge-type :directed)
  (add-edge-between-vertexes g :y :x :edge-type :directed)
  (add-edge-between-vertexes g :w :y :edge-type :directed)
  (add-edge-between-vertexes g :w :z :edge-type :directed)
  (add-edge-between-vertexes g :z :z :edge-type :directed
                             :if-duplicate-do :force)
  (minimum-spanning-tree g))

;;; ---------------------------------------------------------------------------
;;; GWK's implementation of kruskal's - slightly faster, but it doesn't return 
;;; a tree (still faster even if it does).  Will decide later if which to use
;;; ignoring for now -jjm
;;; ---------------------------------------------------------------------------

#+not-yet
(defmethod minimum-spanning-tree ((graph basic-graph) &key (weight 'weight))
  (let ((a nil)
        (union (make-container 'union-find-container))
        (edges (sort (edges graph) #'< :key weight)))
    (iterate-vertexes 
     graph (lambda (v) (insert-item union v)))
    (dolist (edge edges)
      (let ((node-1 (representative-node union (vertex-1 edge)))
            (node-2 (representative-node union (vertex-2 edge))))
        (unless (eq (find-set node-1) (find-set node-2))
          (graft-nodes node-1 node-2)
          (push edge a))))
    
    (values a)))

;;; ---------------------------------------------------------------------------

#+test
(loop for f in '(mst-kruskal minimum-spanning-tree-kruskal) do
      (fluid-bind (((random-seed *random-generator*) 1))
        (bind ((g (generate-undirected-graph-via-vertex-probabilities
                   *random-generator* (make-instance 'graph-container :default-edge-type :directed) 
                   100
                   #(0.8 0.2) 
                   #2A((0.2 0.1) (nil 0.2))
                   (lambda (kind count) 
                     (form-keyword "H" (format nil "~2,'0D~4,'0D" kind count))))
                  ))
          (timeit (:report :values)
                  (loop for n from 1 to 100 do
                        (funcall f g (lambda (a b)
                                       (declare (ignore a b))
                                       0)))))))

;;; ---------------------------------------------------------------------------
;;; end minimum spanning tree
;;; ---------------------------------------------------------------------------

    
;;; ---------------------------------------------------------------------------
;;; depth-first-search - clrs2
;;; todo - figure out how to name this depth-first-search, which is already
;;; defined in search.lisp
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; should probably make this special
;;; ---------------------------------------------------------------------------

(defparameter *depth-first-search-timer* -1)

;;; ---------------------------------------------------------------------------
;;; undirected edges are less than edges that are directed
;;; ---------------------------------------------------------------------------

#+ignore ;;; incorrect, methinks - jjm
(defmethod edge-lessp-by-direction ((e1 basic-edge) (e2 basic-edge))
  (cond ((or (every #'directed-edge-p (list e1 e2))
             (every #'undirected-edge-p (list e1 e2)))
         t)
        ((and (undirected-edge-p e1) (directed-edge-p e2))
         t)
        (t nil)))

(defmethod edge-lessp-by-direction ((e1 basic-edge) (e2 basic-edge))
  (and (undirected-edge-p e1) (directed-edge-p e2)))

;;; ---------------------------------------------------------------------------

(defmethod out-edge-for-vertex-p ((edge basic-edge) (vertex basic-vertex))
  (cond ((and (directed-edge-p edge)
              (eq vertex (source-vertex edge)))
         t)
        ((and (undirected-edge-p edge)
              (or (eq vertex (source-vertex edge))
                  (eq vertex (target-vertex edge))))
         t)
        (t nil)))

;;; ---------------------------------------------------------------------------
;;; depth-first-search
;;; ---------------------------------------------------------------------------
                                                  
(defmethod dfs ((graph basic-graph) (root t) fn &key 
                (out-edge-sorter #'edge-lessp-by-direction))
  (dfs graph (find-vertex graph root) fn :out-edge-sorter out-edge-sorter))

;;; ---------------------------------------------------------------------------

(defmethod dfs ((graph basic-graph) (root basic-vertex) fn &key
                (out-edge-sorter #'edge-lessp-by-direction))
  (setf *depth-first-search-timer* -1)
  
  (iterate-vertexes 
   graph
   (lambda (v)
     (setf (color v) :white
           (previous-node v) nil
           (discovery-time v) -1
           (finish-time v) -1)))
  
  (iterate-edges
   graph
   (lambda (e)
     (setf (color e) nil)))
  
  (loop with vl = (remove root (vertexes graph) :test #'eql)
        for v in (push root vl) do
        (when (eql (color v) :white)
          (dfs-visit graph v fn out-edge-sorter)))
  
  (values
   (sort (copy-list (vertexes graph)) #'< :key #'finish-time)
   graph))

;;; ---------------------------------------------------------------------------

(defmethod dfs-visit ((graph graph-container) (u basic-vertex)
                                     fn sorter)
  
  
  (incf *depth-first-search-timer*)
  (setf (color u) :gray
        (discovery-time u) *depth-first-search-timer*)
  
  
  (loop for edge in (sort (collect-elements
                           (edges u)
                           :filter (lambda (e)
                                     (out-edge-for-vertex-p e u))) sorter) do
        (bind ((v (other-vertex edge u)))
          
          (unless (color edge)
            (setf (color edge) (color v)))
          
          (when (eql (color v) :white)
              (setf (previous-node v) u)
              (funcall fn v)
              (dfs-visit graph v fn sorter))))
  
  (incf *depth-first-search-timer*)
  
  (setf (color u) :black
        (finish-time u) *depth-first-search-timer*))

;;; ---------------------------------------------------------------------------
;;; from clrs2
;;; ---------------------------------------------------------------------------

#+test
(bind ((g (make-container 'graph-container)))
  (add-edge-between-vertexes g :v :y :edge-type :directed)
  (add-edge-between-vertexes g :u :x :edge-type :directed)
  (add-edge-between-vertexes g :x :v :edge-type :directed)
  (add-edge-between-vertexes g :u :v :edge-type :directed)
  (add-edge-between-vertexes g :y :x :edge-type :directed)
  (add-edge-between-vertexes g :w :y :edge-type :directed)
  (add-edge-between-vertexes g :w :z :edge-type :directed)
  (add-edge-between-vertexes g :z :z :edge-type :directed
                             :if-duplicate-do :force)
  (assert (equal '(:X :Y :V :U :Z :W)
                 (mapcar #'element (dfs g :u #'identity)))))

;;; ---------------------------------------------------------------------------

(defmethod dfs-tree-edge-p ((edge graph-container-edge))
  (eql (color edge) :white))

;;; ---------------------------------------------------------------------------

(defmethod dfs-back-edge-p ((edge graph-container-edge))
  (eql (color edge) :gray))

;;; ---------------------------------------------------------------------------
;;; not correct - has to look at combination of discovery-time and finish-time
;;; ---------------------------------------------------------------------------

(defmethod dfs-forward-edge-p ((edge graph-container-edge))
  (warn "implementation is not correct.")
  (unless (and (dfs-tree-edge-p edge)
               (dfs-back-edge-p edge))
    (< (discovery-time (source-vertex edge))
       (discovery-time (target-vertex edge)))))

;;; ---------------------------------------------------------------------------
;;; not correct - has to look at combination of discovery-time and finish-time
;;; ---------------------------------------------------------------------------

(defmethod dfs-cross-edge-p ((edge graph-container-edge))
  (warn "implementation is not correct.")
  (unless (and (dfs-tree-edge-p edge)
               (dfs-back-edge-p edge))
    (> (discovery-time (source-vertex edge))
       (discovery-time (target-vertex edge)))))

;;; ---------------------------------------------------------------------------

(defmethod dfs-edge-type ((edge graph-container-edge))
  (cond ((dfs-tree-edge-p edge)
         :tree)
        ((dfs-back-edge-p edge)
         :back)
        ((dfs-forward-edge-p edge)
         :forward)
        ((dfs-cross-edge-p edge)
         :cross)
        (t nil)))

;;; ---------------------------------------------------------------------------
;;; end dfs
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; mapping functions
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; over vertexes
;;; ---------------------------------------------------------------------------

(defmethod map-over-all-combinations-of-k-vertexes ((graph basic-graph) k fn)
  (bind ((vertex-count (size graph))
         (symbols (make-list k :initial-element vertex-count))
         (vertexes (vertexes graph))) 
    (iterate-over-indexes 
     symbols
     (lambda (vertex-indexes)
       (when (apply #'< vertex-indexes)
         (funcall fn (mapcar (lambda (vertex-index)
                               (nth-element vertexes vertex-index))
                             vertex-indexes)))))))

;;; ---------------------------------------------------------------------------

#+test
(bind ((result nil)
       (g (make-container 'graph-container)))
  (add-edge-between-vertexes g :u :v :edge-type :directed)
  (add-edge-between-vertexes g :u :x :edge-type :directed)
  (add-edge-between-vertexes g :x :v :edge-type :directed)
  (add-edge-between-vertexes g :v :y :edge-type :directed)
  (add-edge-between-vertexes g :y :x :edge-type :directed)
  (add-edge-between-vertexes g :w :y :edge-type :directed)
  (add-edge-between-vertexes g :w :z :edge-type :directed)
  
  (map-over-all-combinations-of-k-vertexes  
   g
   4
   (lambda (vertex-list)
     (bind ((graph-from-vertexes (make-graph-from-vertexes vertex-list)))
       (when (mst-kruskal graph-from-vertexes #'identity-sorter)
         (push graph-from-vertexes result)))))
  result)

;;; ---------------------------------------------------------------------------
;;; over edges 
;;; todo: merge these two defs
;;; ---------------------------------------------------------------------------

(defmethod map-over-all-combinations-of-k-edges ((graph basic-graph) k fn)
  (bind ((edge-count (edge-count graph))
         (symbols (make-list k :initial-element edge-count))
         (edges (edges graph))) 
    (print symbols)
    (iterate-over-indexes 
     symbols
     (lambda (edge-indexes)
       (when (apply #'< edge-indexes)
         (funcall fn (mapcar (lambda (edge-index)
                               (nth-element edges edge-index))
                             edge-indexes)))))))

;;; ---------------------------------------------------------------------------

(defmethod map-over-all-combinations-of-k-edges ((vertex basic-vertex) k fn)
  (bind ((edge-count (edge-count vertex))
         (symbols (make-list k :initial-element edge-count))
         (edges (edges vertex))) 
    (print symbols)
    (iterate-over-indexes 
     symbols
     (lambda (edge-indexes)
       (when (apply #'< edge-indexes)
         (funcall fn (mapcar (lambda (edge-index)
                               (nth-element edges edge-index))
                             edge-indexes)))))))
;;; ---------------------------------------------------------------------------

#+test
(map-over-all-combinations-of-k-edges 
 (generate-undirected-graph-via-verex-probabilities
  *random-generator* 'graph-container 
  10
  #(0.8 0.2) 
  #2A((0.2 0.1) (nil 0.2))
  (lambda (kind count) 
    (form-keyword "H" (format nil "~2,'0D~4,'0D" kind count))))
 2 
 (lambda (es)
   (format t "~%")
   (mapc (lambda (e)
           (format t "~A -- ~A " (element (vertex-1 e)) (element (vertex-2 e))))
         es)))





;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************