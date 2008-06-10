;;;-*- Mode: Lisp; Package: metabang.graph -*-

#| simple-header

$Id: graph-metrics.lisp,v 1.9 2005/08/09 01:56:47 gwking Exp $

Author: Gary King

DISCUSSION

|#
(in-package #:metabang.graph)

(eval-always 
  (import '(cl-mathstats:matrix-trace
	    cl-mathstats:sum-of-array-elements
	    cl-mathstats:matrix-multiply
	    cl-mathstats:normalize-matrix
	    cl-mathstats:combination-count
	    )))

(defun vertex-degree-counts (g)
  "Returns an associative-container mapping edge-counts to the number of vertexes with that edge-count."
  (let ((c (make-container 'associative-container :initial-element 0)))
    (iterate-vertexes 
     g
     (lambda (v)
       (incf (item-at c (edge-count v)))))
    c))

;;; ---------------------------------------------------------------------------

(defun average-vertex-degree (graph &key 
                                (vertex-filter (constantly t))
                                (edge-filter (constantly t))
                                (edge-size (constantly 1)))
  "Returns the average degree of the all of the vertexes in `graph` that pass the `vertex-filter`. Both `vertex-filter` and `edge-filter` are predicates; `edge-size` is a function that maps edges to their weight.  Compare with `vertex-degree`."
  (let ((total 0)
        (size 0))
    (iterate-container 
     graph
     (lambda (v) 
       (when (funcall vertex-filter v)
         (incf size)
         (incf total (%vertex-degree v edge-filter edge-size)))))
    (if size
      (values (float (/ total size)))
      nil)))

;;; ---------------------------------------------------------------------------

(defun vertex-degree (vertex &key 
                             (edge-filter (constantly t))
                             (edge-size (constantly 1)))
  "Returns the degree of `vertex`. The degree is computed by totaling the `edge-size` \(e.g., the `weight`\) of each edge attached to vertex that passes `edge-filter`. `Edge-filter is a predicate and `edge-size` should map edges to their weights."
  (declare (inline %vertex-degree)) 
  (%vertex-degree vertex edge-filter edge-size))

;;; ---------------------------------------------------------------------------

(defun %vertex-degree (vertex edge-filter edge-size)
  "Called internally by `vertex-degree` and `average-vertex-degree`."
  (let ((degree 0))
    (iterate-edges 
     vertex
     (lambda (e)
       (when (funcall edge-filter e (other-vertex e vertex))
         (incf degree (funcall edge-size e)))))
    degree))

;;; ---------------------------------------------------------------------------

(defun vertex-degree-summary (graph vertex-classifier
                                    &key (edge-size (constantly 1)))
  "Prints a summary of vertex degrees in `graph` to standard-out. Both the average degree of all vertexes and the average degree between all pairs of vertex classes \(as determined by the vertex-classifier\) will be printed. The `edge-size` parameter is passed on to `vertex-degree` to allow for weighted edges."
  
  (bind ((counts (node-counts graph :key vertex-classifier))
         (kinds (collect-elements counts :transform #'first)))
    (format t "~%Vertex counts: ")
    (loop for (kind count) in counts do
          (format t "~A = ~A; " kind count))
    (flet ((show-avd (vertex-filter edge-filter message &rest args)
             (terpri)
             (apply #'format t message args)
             (format t "~7,2F"
                     (average-vertex-degree 
                      graph
                      :vertex-filter (or vertex-filter (constantly t))
                      :edge-filter (or edge-filter (constantly t))
                      :edge-size edge-size))))
      (show-avd nil nil "Average vertex degree:")
      (loop for kind in kinds do 
            (show-avd (lambda (v) (equal kind (funcall vertex-classifier v))) nil
                      "Average vertex degree for ~A:" kind))
      (dolist (k-1 kinds)
        (dolist (k-2 kinds)
          (show-avd
           (lambda (v) (equal (funcall vertex-classifier v) k-1))
           (lambda (e v)
             (declare (ignore e))
             (equal (funcall vertex-classifier v) k-2))
           "Average vertex degree between ~A and ~A:"
           k-1 k-2))))))

;;; ---------------------------------------------------------------------------

#|
Transitivity or Clustering.
the friend of your friend is likely also to be your friend.

C = 3 x number of triangles in the network /
    number of connected triples of vertices
    
or

C = 6 x number of triangles in the network /
    number of paths of length two

C measures the fraction of triples that have their third edge *filled in
to complete the triangle.

The definition of C given here has been widely used in the sociology literature,
where it is referred to as the  fraction of transitive triples. 

An alternative definition of the clustering coefficient, also widely used, has been given by Watts and Strogatz [415], who proposed de*ning a local value

Ci = number of triangles connected to vertex i /
     number of triples centered on vertex i

For vertices with degree 0 or 1, for which both numerator and denominator are zero, 
we put Ci = 0.

C = Sum( Ci ) / n

It tends to weight the contributions of
low-degree vertices more heavily, because such vertices have a small denominator in (3.5) and hence can give quite di*erent results from (3.3).

The local clustering Ci above has been used quite widely in its own right in
the sociological literature, where it is referred to as the network density
|#

(defun average-vertex-clustering-coefficient (graph)
  "Returns the average `vertex-clustering-coefficient` of all the vertexes in the graph."
  (/ 
   (let ((total 0.0)) 
     (iterate-vertexes 
      graph (lambda (v) (incf total (vertex-clustering-coefficient v))))
     total)
   (size graph)))

;;; ---------------------------------------------------------------------------    

(defun vertex-clustering-coefficient (vertex)
  "The vertex-clustering-coefficient is, informally, a measure of the number of triangles in which a vertex participates as compared to the maximum possible number of triangles in which it could participate. It measures how likely it is that any two neighbors of the vertex are also joined by an edge."
  (if (< (edge-count vertex) 2)
    0.0
    (float (/ (vertex-triangle-count vertex)
              (combination-count (edge-count vertex) 2)))))

;;; ---------------------------------------------------------------------------

(defun vertex-triangle-count (vertex)
  (let ((neighbors (neighbor-vertexes vertex)))
    (loop for neighbor in neighbors sum
          (/ (count-if (lambda (v)
                         (member v neighbors))
                       (neighbor-vertexes neighbor)) 2))))

;;; ---------------------------------------------------------------------------

(defun row-sums (matrix)
  (let* ((row-count (array-dimension matrix 1))
         (result (make-array row-count :initial-element 0d0)))
    (dotimes (row row-count)
      (dotimes (column (array-dimension matrix 0))
        (incf (aref result row) (aref matrix column row))))
    result))

;;; ---------------------------------------------------------------------------

(defun column-sums (matrix)
  (let* ((column-count (array-dimension matrix 0))
         (result (make-array column-count :initial-element 0d0)))
    (dotimes (column column-count)
      (dotimes (row (array-dimension matrix 1))
        (incf (aref result column) (aref matrix column row))))
    result))

;;; ---------------------------------------------------------------------------

(defmethod assortativity-coefficient ((matrix array))
  
  (let* ((matrix (normalize-matrix matrix))
         (sum-squared (sum-of-array-elements (matrix-multiply matrix matrix)))
         (trace (matrix-trace matrix)))
    (if (= trace 1d0)
      (values 1)
      (values (/ (- trace sum-squared) (- 1 sum-squared))))))

;;; ---------------------------------------------------------------------------

(defmethod graph-edge-mixture-matrix ((graph basic-graph) vertex-classifier &key
                                (edge-weight (constantly 1)))
  (let* ((vertex-types (remove-duplicates
                        (collect-items graph :transform vertex-classifier)))
         (size (size vertex-types))
         (matrix (make-array (list size size) :initial-element 0d0)))
    (iterate-edges 
     graph
     (lambda (e)
       (let* ((vertex-class-1 (funcall vertex-classifier (vertex-1 e)))
              (vertex-class-2 (funcall vertex-classifier (vertex-2 e)))
              (index-1 (position vertex-class-1 vertex-types))
              (index-2 (position vertex-class-2 vertex-types))
              (weight (funcall edge-weight e)))
         (incf (aref matrix index-1 index-2) weight)
         (incf (aref matrix index-2 index-1) weight))))
    (values 
     (matrix-multiply matrix (/ (sum-of-array-elements matrix)))
     vertex-types)))

#+Test
(assortativity-coefficient
 #2A((0.258 0.016 0.035 0.013)
     (0.012 0.157 0.058 0.019)
     (0.013 0.023 0.306 0.035)
     (0.005 0.007 0.024 0.016)))

;;; ---------------------------------------------------------------------------

;;OPT we call the classifier a lot, probably better to make a new ht for that

(defmethod graph-mixing-matrix ((graph basic-graph) vertex-classifier &key
                                (edge-weight (constantly 1)))
  (declare (ignore edge-weight))
  (let* ((vertex-types (remove-duplicates
                        (collect-items graph :transform vertex-classifier)))
         (size (size vertex-types))
         (matrix (make-array (list size size) :initial-element 0d0))
         (class-sizes (make-container 'simple-associative-container 
                                      :initial-element 0
                                      :test #'eq))
         (class-indexes (make-container 'simple-associative-container 
                                        :initial-element nil
                                        :test #'eq)))
    (block determine-class-indexes
      (let ((n -1))
        (iterate-vertexes
         graph
         (lambda (v)
           (let ((vertex-class (funcall vertex-classifier v)))
             (unless (item-at-1 class-indexes vertex-class)
               (setf (item-at-1 class-indexes vertex-class) (incf n))
               (when (= n (1- size))
                 (return-from determine-class-indexes nil))))))))
    
    (iterate-vertexes
     graph
     (lambda (v)
       (incf (item-at-1 class-sizes (funcall vertex-classifier v)))))
    
    (iterate-vertexes
     graph
     (lambda (v-1)
       (let ((index-1 (item-at-1 class-indexes (funcall vertex-classifier v-1))))
         (iterate-neighbors 
          v-1
          (lambda (v-2)
            (let ((index-2 (item-at-1 class-indexes (funcall vertex-classifier v-2))))
              (incf (item-at matrix index-1 index-2))))))))
    
    #+Ignore
    (iterate-key-value
     class-indexes
     (lambda (class-1 index-1)
       (iterate-key-value
        class-indexes
        (lambda (class-2 index-2)
          (setf (item-at matrix index-1 index-2)
                (/ (item-at matrix index-1 index-2)
                   (if (= index-1 index-2) 
                     (* 2 (combination-count (item-at-1 class-sizes class-1) 2))
                     (combination-count (+ (item-at-1 class-sizes class-1) 
                                           (item-at-1 class-sizes class-2))
                                        2))))))))
    
    (values matrix (collect-key-value class-indexes)
            (collect-key-value class-sizes))))

#+Test
;; this computes the same matrix but is probably slower and more consy
(time
 (let ((vertex-classes 
        (merge-nodes
         (adma::ds :g-5000)
         (lambda (old new)
           (push new old))
         (lambda (first)
           (list first))
         :key (lambda (v) (aref (symbol-name (element v)) 0)))))
   (loop for (class vertexes) in vertex-classes collect
         (list class 
               (element-counts
                (loop for vertex in vertexes append
                      (neighbor-vertexes vertex))
                :key (lambda (v) (aref (symbol-name (element v)) 0)))))))

#+Old
(defmethod graph-mixing-matrix ((graph basic-graph) vertex-classifier &key
                                (edge-weight (constantly 1)))
  (let* ((vertex-types (remove-duplicates
                        (collect-items graph :transform vertex-classifier)))
         (size (size vertex-types))
         (matrix (make-array (list size size) :initial-element 0d0))
         (class-sizes (make-container 'simple-associative-container 
                                      :initial-element 0
                                      :test #'eq))
         (class-indexes (make-container 'simple-associative-container 
                                        :initial-element nil
                                        :test #'eq)))
    (block determine-class-indexes
      (let ((n -1))
        (iterate-vertexes
         graph
         (lambda (v)
           (let ((vertex-class (funcall vertex-classifier v)))
             (unless (item-at-1 class-indexes vertex-class)
               (setf (item-at-1 class-indexes vertex-class) (incf n))
               (when (= n (1- size))
                 (return-from determine-class-indexes nil))))))))
    
    (iterate-vertexes
     graph
     (lambda (v)
       (incf (item-at-1 class-sizes 
                        (item-at-1 class-indexes (funcall vertex-classifier v))))))
    
    (iterate-vertexes
     graph
     (lambda (v-1)
       (let ((index-1 (item-at-1 class-indexes (funcall vertex-classifier v-1))))
         (iterate-neighbors 
          v-1
          (lambda (v-2)
            (let ((index-2 (item-at-1 class-indexes (funcall vertex-classifier v-2))))
              ;  (when (= index-1 1 index-2)
              ;    (break))
              ;  (when (< index-2 index-1)
              ;    (rotatef index-1 index-2))
              (unless (< index-2 index-1)
                (incf (item-at matrix index-1 index-2)))))))))
    
    (iterate-key-value
     class-indexes
     (lambda (class-1 index-1)
       (iterate-key-value
        class-indexes
        (lambda (class-2 index-2)
          (when (<= index-1 index-2)
            (setf (item-at matrix index-1 index-2)
                  (/ (item-at matrix index-1 index-2)
                     (if (= index-1 index-2) 
                       (* 2 (combination-count (item-at-1 class-sizes class-1) 2))
                       (/ (combination-count (+ (item-at-1 class-sizes class-1) 
                                                (item-at-1 class-sizes class-2))
                                             2) 2)))))))))
    
    (values matrix (collect-key-value class-indexes))))
              



              
              
              