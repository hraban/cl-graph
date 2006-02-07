(in-package metabang.graph)

(export '(generate-Gnp
          generate-Gnm
          generate-undirected-graph-via-assortativity-matrix
          generate-undirected-graph-via-vertex-probabilities
          generate-multi-group-graph-fixed
          #+Ignore generate-girvan-newman-graph
          generate-scale-free-graph
          generate-assortative-graph-with-degree-distributions
          
          generate-simple-preferential-attachment-graph
          generate-preferential-attachment-graph
          
          generate-acquaintance-network
          generate-acquaintance-network-until-stable

          generate-graph-by-resampling-edges

          sample-edge
          basic-edge-sampler
          weighted-edge-sampler
          simple-group-id-generator
          simple-group-id-parser
          
          make-degree-sampler
          poisson-vertex-degree-distribution
          power-law-vertex-degree-distribution))

;;; ---------------------------------------------------------------------------
;;; classes
;;; ---------------------------------------------------------------------------

(defclass* generated-graph-mixin ()
  ((generation-method nil ir)
   (random-seed nil ir)))

;;; ---------------------------------------------------------------------------

(defun save-generation-information (graph generator method)
  ;; No
  ;; (setf (random-seed generator) (random-seed generator)) 
  (unless (typep graph 'generated-graph-mixin)
    (change-class graph (find-or-create-class
                         'basic-graph (list 'generated-graph-mixin
                                            (class-name (class-of graph))))))
  (setf (slot-value graph 'generation-method) method
        (slot-value graph 'random-seed) (random-seed generator)))

;;; ---------------------------------------------------------------------------

(defun simple-group-id-generator (kind count) 
  (form-keyword "H" (format nil "~2,'0D~4,'0D" kind count)))

;;; ---------------------------------------------------------------------------

(defun simple-group-id-parser (vertex) 
  (parse-integer (subseq (symbol-name (element vertex)) 1 3)))


;;; ---------------------------------------------------------------------------
;;; generate-Gnp
;;; ---------------------------------------------------------------------------

(defmethod generate-Gnp (generator (graph-class symbol) n p &key (label 'identity))
  (generate-Gnp
   generator (make-instance graph-class) n p :label label))

;;; ---------------------------------------------------------------------------

(defmethod generate-Gnp (generator (graph basic-graph) n p &key (label 'identity))
  (let ((v 1)
        (w -1)
        (log-1-p (log (- 1 p))))
    (save-generation-information graph generator 'generate-gnp)
    (loop for i from 0 to (1- n) do
          (add-vertex graph (funcall label i)))
    (loop while (< v n) do
          (let ((r (uniform-random generator 0d0 1d0)))
            (setf w (+ w 1 (floor (/ (log (- 1 r)) log-1-p))))
            (loop while (and (>= w v) (< v n)) do
                  (setf w (- w v) 
                        v (1+ v)))
            (when (< v n) 
              (add-edge-between-vertexes 
               graph (funcall label v) (funcall label w)))))
    
    graph))

;;; ---------------------------------------------------------------------------
;;; generate-Gnm
;;; ---------------------------------------------------------------------------

(defmethod generate-Gnm (generator (graph-class symbol) n p &key (label 'identity))
  (generate-Gnm
   generator (make-instance graph-class) n p :label label))

;;; ---------------------------------------------------------------------------

(defmethod generate-Gnm (generator (graph basic-graph) n m &key (label 'identity))
  (let ((max-edge-index (1- (combination-count n 2))))
    #+Ignore
    (save-generation-information graph generator 'generate-gnm)
    (loop for i from 0 to (1- n) do
          (add-vertex graph (funcall label i)))
    (loop for i from 0 to (1- m) do
          (loop 
            until (let* ((i (integer-random generator 0 max-edge-index))
                         (v (1+ (floor (+ -0.5 (sqrt (+ 0.25 (* 2 i)))))))
                         (w (- i (/ (* v (1- v)) 2)))
                         (label-v (funcall label v))
                         (label-w (funcall label w)))
                    (unless (find-edge-between-vertexes 
                             graph label-v label-w :error-if-not-found? nil)
                      (add-edge-between-vertexes graph label-v label-w)))))
    
    graph))

#+Ignore
(pro:with-profiling
  (setf g (generate-gnm 
           *random-generator*
           'graph-container 10000 (floor (* 0.0001 (combination-count 10000 2)))))
  )
        
;;; ---------------------------------------------------------------------------
         
(defun vertex-group (v)
  (aref (symbol-name (element v)) 1))

;;; ---------------------------------------------------------------------------

(defun in-group-degree (v &key (key 'vertex-group))
  (vertex-degree 
   v :edge-filter (lambda (e ov) 
                    (declare (ignore e))
                    (in-same-group-p v ov key))))

;;; ---------------------------------------------------------------------------

(defun in-same-group-p (v1 v2 key)
  (eq (funcall key v1) (funcall key v2)))

;;; ---------------------------------------------------------------------------

(defun out-group-degree (v &key (key 'vertex-group))
  (vertex-degree 
   v :edge-filter (lambda (e ov) 
                    (declare (ignore e))
                    (not (in-same-group-p v ov key)))))

;;; ---------------------------------------------------------------------------
;;; generate-undirected-graph-via-assortativity-matrix 
;;; ---------------------------------------------------------------------------

(defmethod generate-undirected-graph-via-assortativity-matrix
           (generator (graph-class symbol) size edge-count 
                      kind-matrix assortativity-matrix vertex-creator
                      &key (duplicate-edge-function 'identity))
  (generate-undirected-graph-via-assortativity-matrix
   generator (make-instance graph-class)  size edge-count 
   kind-matrix assortativity-matrix vertex-creator
   :duplicate-edge-function duplicate-edge-function))

;;; ---------------------------------------------------------------------------

(defmethod generate-undirected-graph-via-assortativity-matrix
           (generator graph size edge-count 
                      kind-matrix assortativity-matrix vertex-creator
                      &key (duplicate-edge-function 'identity))
  (let* ((kind-count (array-dimension assortativity-matrix 0))
         (vertex-kinds (sort (sample-vertexes-for-mixed-graph generator size kind-matrix) 
                             #'<))
         (vertex-kind-counts (element-counts vertex-kinds :sort #'< :sort-on :values))
         (vertex-sampler (make-array kind-count))
         (edge-kinds (sample-edges-for-assortative-graph 
                      generator edge-count assortativity-matrix))
         )
    (save-generation-information graph generator 'generate-undirected-graph-via-assortativity-matrix)
    
    (loop for vertex-kind from 0 to (1- kind-count) 
          for count in vertex-kind-counts do
          (setf (aref vertex-sampler vertex-kind) 
                (make-array (second count))))
    
    (let ((current-kind 0)
          (current-count 0)
          (current-vertexes (aref vertex-sampler 0)))
      ;; add vertexes
      (loop for kind in vertex-kinds 
            for i from 0 do 
            (when (not (eq current-kind kind))
              (setf current-count 0 
                    current-kind kind
                    current-vertexes (aref vertex-sampler current-kind)))
            (let ((vertex (funcall vertex-creator kind i)))
              (setf (aref current-vertexes current-count) vertex)
              (add-vertex graph vertex)
              (incf current-count)))
      
      (loop for (from-kind to-kind) in edge-kinds do
            (let ((v1 nil) 
                  (v2 nil))
              (if (= from-kind to-kind)
                (let ((sample (sample-unique-elements (aref vertex-sampler from-kind)
                                                      generator 2)))
                  (setf v1 (first sample) v2 (second sample)))
                (setf v1 (sample-element (aref vertex-sampler from-kind) generator)
                      v2 (sample-element (aref vertex-sampler to-kind) generator)))
              (add-edge-between-vertexes 
               graph 
               v1
               v2
               :if-duplicate-do (lambda (e) (funcall duplicate-edge-function e))))))
      
      (values graph)))

;;; ---------------------------------------------------------------------------
;;; generate-undirected-graph-via-verex-probabilities
;;; ---------------------------------------------------------------------------

(defmethod generate-undirected-graph-via-vertex-probabilities
           (generator (graph-class symbol) size 
                      kind-matrix probability-matrix vertex-creator)
  (generate-undirected-graph-via-vertex-probabilities
   generator (make-instance graph-class) size 
   kind-matrix probability-matrix vertex-creator))

;;; ---------------------------------------------------------------------------

(defmethod generate-undirected-graph-via-vertex-probabilities
           (generator graph size 
                      kind-matrix probability-matrix vertex-creator)
  (let* ((kind-count (array-dimension probability-matrix 0))
         (vertex-kinds (sort (sample-vertexes-for-mixed-graph generator size kind-matrix) 
                             #'<))
         (vertex-kind-counts (element-counts vertex-kinds :sort #'< :sort-on :values))
         (vertex-sampler (make-array kind-count)))
    (save-generation-information graph generator 
                                 'generate-undirected-graph-via-vertex-probabilities)
    
    ;; initialize vertex bookkeeping 
    (loop for vertex-kind from 0 to (1- kind-count) 
          for count in vertex-kind-counts do
          (setf (aref vertex-sampler vertex-kind) 
                (make-array (second count))))
    
    ;; add vertexes
    (let ((current-kind 0)
          (current-count 0)
          (current-vertexes (aref vertex-sampler 0)))
      (loop for kind in vertex-kinds 
            for i from 0 do 
            (when (not (eq current-kind kind))
              (setf current-count 0 
                    current-kind kind
                    current-vertexes (aref vertex-sampler current-kind)))
            (let ((vertex (funcall vertex-creator kind i)))
              (setf (aref current-vertexes current-count) vertex)
              (add-vertex graph vertex)
              (incf current-count))))
    
    #+Ignore
    ;; adjust probabilities
    (loop for (kind-1 count-1) in vertex-kind-counts do
          (loop for (kind-2 count-2) in vertex-kind-counts 
                when (<= kind-1 kind-2) do
                (format t "~%~6,6F ~6,6F" 
                        (aref probability-matrix kind-1 kind-2)
                        (float (/ (aref probability-matrix kind-1 kind-2) 
                                  (* count-1 count-2))))
                (setf (aref probability-matrix kind-1 kind-2)
                      (float (/ (aref probability-matrix kind-1 kind-2) 
                                (* count-1 count-2))))))
    
    ;; add edges
    (flet ((add-one-edge (k1 k2 a b) 
             (add-edge-between-vertexes 
              graph
              (aref (aref vertex-sampler k1) a)
              (aref (aref vertex-sampler k2) b))))
      (loop for (kind-1 count-1) in vertex-kind-counts do
            (loop for (kind-2 count-2) in vertex-kind-counts 
                  when (<= kind-1 kind-2) do
                  (if (eq kind-1 kind-2)
                    (sample-edges-of-same-kind 
                     generator count-1 (aref probability-matrix kind-1 kind-2)
                     (lambda (a b)
                       (add-one-edge kind-1 kind-2 a b)))
                    (sample-edges-of-different-kinds 
                     generator count-1 count-2 (aref probability-matrix kind-1 kind-2)
                     (lambda (a b)
                       (add-one-edge kind-1 kind-2 a b)))))))
    (values graph)))


#+Debug
(defmethod generate-undirected-graph-via-vertex-probabilities
           (generator graph size 
                      kind-matrix probability-matrix vertex-creator)
  (let* ((kind-count (array-dimension probability-matrix 0))
         (vertex-kinds (sort (sample-vertexes-for-mixed-graph generator size kind-matrix) 
                             #'<))
         (vertex-kind-counts (element-counts vertex-kinds :sort #'< :sort-on :values))
         (vertex-sampler (make-array kind-count)))
    
    (loop for vertex-kind from 0 to (1- kind-count) 
          for count in vertex-kind-counts do
          (setf (aref vertex-sampler vertex-kind) 
                (make-array (second count))))
    
    (let ((current-kind 0)
          (current-count 0)
          (current-vertexes (aref vertex-sampler 0)))
      ;; add vertexes
      (loop for kind in vertex-kinds 
            for i from 0 do 
            (when (not (eq current-kind kind))
              (setf current-count 0 
                    current-kind kind
                    current-vertexes (aref vertex-sampler current-kind)))
            (let ((vertex (funcall vertex-creator kind i)))
              (setf (aref current-vertexes current-count) vertex)
              (add-vertex graph vertex)
              (incf current-count))))
    
    (let ((xxx 0))
      (flet ((add-one-edge (k1 k2 a b) 
               (incf xxx)
               (add-edge-between-vertexes 
                graph
                (aref (aref vertex-sampler k1) a)
                (aref (aref vertex-sampler k2) b))))
        (loop for (kind-1 count-1) in vertex-kind-counts do
              (loop for (kind-2 count-2) in vertex-kind-counts 
                    when (<= kind-1 kind-2) do
                    (setf xxx 0)
                    (if (eq kind-1 kind-2)
                      (sample-edges-of-same-kind 
                       generator count-1 (aref probability-matrix kind-1 kind-2)
                       (lambda (a b)
                         (add-one-edge kind-1 kind-2 a b)))
                      (sample-edges-of-different-kinds 
                       generator count-1 count-2 (aref probability-matrix kind-1 kind-2)
                       (lambda (a b)
                         (add-one-edge kind-1 kind-2 a b))))
                    (format t "~%~A ~A ~A ~A -> ~A"
                            count-1 count-2 kind-1 kind-2 xxx)))))
    (values graph)))


#+Test
(generate-undirected-graph-via-vertex-probabilities
 *random-generator* 'graph-container 
 30 
 #(0.8 0.2) 
 #2A((0.1 0.02) (0.02 0.6))
 (lambda (kind count) 
   (form-keyword "H" (format nil "~2,'0D~4,'0D" kind count))))

;;; ---------------------------------------------------------------------------

(defun sample-edges-of-same-kind (generator n p fn)
  (when (plusp p)
    (let ((v 1)
          (w -1)
          (log-1-p (log (- 1 p))))
      (loop while (< v n) do
            (let ((r (uniform-random generator 0d0 1d0)))
              (setf w (+ w 1 (floor (/ (log (- 1 r)) log-1-p))))
              (loop while (and (>= w v) (< v n)) do
                    (setf w (- w v) 
                          v (1+ v)))
              (when (< v n) 
                (funcall fn v w)))))))

#+Test
(sample-edges-of-same-kind *random-generator* 10 0.2 (lambda (a b) (print (list a b))))

;;; ---------------------------------------------------------------------------

(defun sample-edges-of-different-kinds (generator rows cols p fn)
  (when (plusp p)
    (let ((v 1)
          (w -1)
          (log-1-p (log (- 1 p))))
      (loop while (< v rows) do
            (let ((r (uniform-random generator 0d0 1d0)))
              (setf w (+ w 1 (floor (/ (log (- 1 r)) log-1-p))))
              (loop while (and (>= w cols) (< v rows)) do
                    (setf w (- w cols) 
                          v (1+ v)))
              (when (< v rows) 
                (funcall fn v w))))))) 

;;; ---------------------------------------------------------------------------

(defun poisson-vertex-degree-distribution (z k)
  (/ (* (expt z k) (expt +e+ (- z)))
     (factorial k)))

#|
We know the probability of finding a vertex of degree k is p_k. We want to sample
from this distribution
|#

;;; ---------------------------------------------------------------------------

(defun power-law-vertex-degree-distribution (kappa k)
  (* (- 1 (expt +e+ (- (/ kappa)))) (expt +e+ (- (/ k kappa)))))

;;; ---------------------------------------------------------------------------

(defun create-specified-vertex-degree-distribution (degrees)
  (lambda (z k)
    (declare (ignore z k))
    degrees))

;;; ---------------------------------------------------------------------------

(defun make-degree-sampler (p_k &key (generator *random-generator*)
                                (max-degree 1000)
                                (min-probability 0.0001))
  (let ((wsc (make-container 'containers:weighted-sampling-container
                             :random-number-generator generator
                             :key #'second))
        (total 0.0)
        (max-k 0))
    (loop for k = 0 then (1+ k)
          for p = (funcall p_k k) 
          until (or (and max-degree (> k max-degree))
                    (and min-probability (< (- 1.0 total) min-probability))) do
          (incf total p)
          (setf max-k k)
          (insert-item wsc (list k p)))
    (when (plusp (- 1.0 total))
      (insert-item wsc (list (1+ max-k) (- 1.0 total))))
    (lambda ()
      (first (next-element wsc)))))

;;; ---------------------------------------------------------------------------

#+Old
(defun sample-edges-for-assortative-graph (generator edge-count assortativity-matrix)
  (let ((c (make-container 'weighted-sampling-container
                           :random-number-generator generator
                           :key (lambda (item)
                                  (aref assortativity-matrix (first item) (second item))))))
    (dotimes (i (array-dimension assortativity-matrix 0))
      (dotimes (j (array-dimension assortativity-matrix 1)) 
        (insert-item c (list i j))))
    (loop repeat edge-count collect
          (next-element c))))

;;; ---------------------------------------------------------------------------

(defun sample-edges-for-assortative-graph (generator edge-count assortativity-matrix)
  (let ((s (make-edge-sampler-for-assortative-graph generator assortativity-matrix)))
    (loop repeat edge-count collect
          (funcall s))))

;;; ---------------------------------------------------------------------------

(defun make-edge-sampler-for-assortative-graph (generator assortativity-matrix)
  (let ((c (make-container 'weighted-sampling-container
                           :random-number-generator generator
                           :key (lambda (item)
                                  (aref assortativity-matrix (first item) (second item))))))
    (dotimes (i (array-dimension assortativity-matrix 0))
      (dotimes (j (array-dimension assortativity-matrix 1)) 
        (insert-item c (list i j))))
    (lambda () (next-element c))))

;;; ---------------------------------------------------------------------------

(defun sample-vertexes-for-mixed-graph (generator size kind-matrix)
  (cond ((every-element-p kind-matrix (lambda (x) (fixnump x)))
         ;; use kind-matrix as counts
         (assert (= size (sum-of-array-elements kind-matrix)))
         (coerce (shuffle-elements! 
                  (make-array size 
                              :initial-contents
                              (loop for i = 0 then (1+ i) 
                                    for count across kind-matrix nconc
                                    (make-list count :initial-element i)))
                  :generator generator)
                 'list))
        
        (t
         ;; use kind-matrix as ratios to sample
         (let* ((c (make-container 'weighted-sampling-container
                                   :random-number-generator generator
                                   :key (lambda (item)
                                          (aref kind-matrix item)))))
           (dotimes (i (array-dimension kind-matrix 0))
             (insert-item c i))
           (loop repeat size collect
                 (next-element c))))))

#+Test
(sample-vertexes-for-mixed-graph 
 *random-generator*
 50 #2A((0.258 0.016 0.035 0.013)
        (0.012 0.157 0.058 0.019)
        (0.013 0.023 0.306 0.035)
        (0.005 0.007 0.024 0.016)))

#+Test
(sample-edges 50 #2A((0.258 0.016 0.035 0.013)
                     (0.012 0.157 0.058 0.019)
                     (0.013 0.023 0.306 0.035)
                     (0.005 0.007 0.024 0.016)))
#+Test
(let ((a #2A((0.258 0.016 0.035 0.013)
             (0.012 0.157 0.058 0.019)
             (0.013 0.023 0.306 0.035)
             (0.005 0.007 0.024 0.016)))
      (c (make-container 'weighted-sampling-container :key #'second)))
  (dotimes (i 4)
    (dotimes (j 4) 
      (insert-item c (list (list i j) (aref a i j)))))
  (element-counts
   (loop repeat 1000 collect
         (next-element c))
   :key #'first
   :test #'equal))
      
#+Test
(let ((a #2A((0.258 0.016 0.035 0.013)
             (0.012 0.157 0.058 0.019)
             (0.013 0.023 0.306 0.035)
             (0.005 0.007 0.024 0.016)))
      (c (make-container 'weighted-sampling-container :key #'second)))
  (pro:with-profiling
    (loop repeat 100000 do
          (next-element c))))
      
#+Test
(defun foo (percent-bad percent-mixing)
  (let ((kind-matrix (make-array 2 :initial-element 0d0))
        (mixing-matrix (make-array (list 2 2) :initial-element 0d0)))
    (setf (aref kind-matrix 0) (- 1d0 percent-bad)
          (aref kind-matrix 1) percent-bad
          (aref mixing-matrix 0 0) (* (aref kind-matrix 0) (- 1d0 (/ percent-mixing 1)))
          (aref mixing-matrix 1 1) (* (aref kind-matrix 1) (- 1d0 (/ percent-mixing 1)))
          (aref mixing-matrix 1 0) percent-mixing
          (aref mixing-matrix 0 1) percent-mixing)
    (normalize-matrix kind-matrix)
    (setf mixing-matrix (normalize-matrix mixing-matrix))
    (values kind-matrix 
            mixing-matrix)))


;;; ---------------------------------------------------------------------------
;;; girvan-newman-test-graphs
;;; ---------------------------------------------------------------------------

(defun generate-girvan-newman-graph (generator graph-class z-in)
  (warn "This is broken!")
  (bind ((g (make-instance graph-class))
         (group-count 4)
         (group-size 32)
         (edge-count 16)
         (z-out (- edge-count z-in))
         (vertexes (make-container 'simple-associative-container))
         (groups (make-container 'alist-container)))
    (save-generation-information g generator 
                                 'generate-girvan-newman-graph)
    (labels ((make-id (group index)
               (form-keyword "A" group "0" index))
             
             (choose-inner-id (group id)
               (check-type group fixnum)
               (check-type id symbol)
               (loop 
                 (let ((other (sample-element (item-at groups group :needs-in) generator)))
                   (when (and #+Ignore
                              (not (eq id other))
                              #+Ignore
                              (not (find-edge-between-vertexes
                                    g id other :error-if-not-found? nil)))
                     (return-from choose-inner-id other)))))
             
             (choose-outer-id (from-group id)
               (declare (ignore id))
               
               (check-type from-group fixnum)
               (loop 
                 (bind ((other-group (integer-random generator 0 (- group-count 2)))
                        (other (sample-element 
                                (item-at groups (if (= from-group other-group)
                                                  (1+ other-group)
                                                  other-group) :needs-out)
                                generator)))
                   (when (and other
                              #+Ignore
                              (not (find-edge-between-vertexes 
                                    g id other :error-if-not-found? nil)))
                     (return-from choose-outer-id other)))))
             
             (make-in-edge (from to)
               (let ((group (gn-id->group from)))
                 (when (zerop (decf (first (item-at vertexes from))))
                   (setf (item-at groups group :needs-in)
                         (remove from (item-at groups group :needs-in))))
                 (when (zerop (decf (first (item-at vertexes to))))
                   (setf (item-at groups group :needs-in)
                         (remove to (item-at groups group :needs-in))))
                 (add-edge-between-vertexes
                  g from to :edge-type :undirected 
                  :if-duplicate-do (lambda (e) (incf (weight e))))))
             
             (make-out-edge (from to)
               (let ((group-from (gn-id->group from))
                     (group-to (gn-id->group to)))
                 (when (zerop (decf (second (item-at vertexes from))))
                   (setf (item-at groups group-from :needs-out)
                         (remove from (item-at groups group-from :needs-out))))
                 (when (zerop (decf (second (item-at vertexes to))))
                   (setf (item-at groups group-to :needs-out)
                         (remove to (item-at groups group-to :needs-out))))
                 
                 (add-edge-between-vertexes
                  g from to :edge-type :undirected
                  :if-duplicate-do (lambda (e) (incf (weight e)))))))
      
      ;; vertexes
      (loop for group from 0 to (1- group-count) do
            (loop for index from 0 to (1- group-size) do
                  (let ((id (make-id group index)))
                    (setf (item-at vertexes id) (list z-in z-out))
                    (when (plusp z-in)
                      (push id (item-at groups group :needs-in)))
                    (when (plusp z-out)
                      (push id (item-at groups group :needs-out))))))
     
      ;; create edges
      (loop for group from 0 to (1- group-count) do
            (loop for index from 0 to (1- group-size) do
                  (let ((from (make-id group index)))
                    (print from)
                    (loop while (plusp (first (item-at vertexes from))) do
                          (make-in-edge from (choose-inner-id group from)))
                    (loop while (plusp (second (item-at vertexes from))) do
                          (make-out-edge from (choose-outer-id group from)))))))
  
  (values g)))

;;; ---------------------------------------------------------------------------

(defun gn-id->group (id)
  (parse-integer (subseq (symbol-name id) 1 2)))

;;; ---------------------------------------------------------------------------

(defun collect-edge-counts (g)
  (let ((vertexes (make-container 'simple-associative-container 
                                  :initial-element-fn (lambda () (list 0 0)))))
    (iterate-edges
     g
     (lambda (e)
       (bind ((v1 (vertex-1 e))
              (v2 (vertex-2 e))
              (id1 (element v1))
              (id2 (element v2)))
         (cond ((= (gn-id->group id1) (gn-id->group (element v2)))
                (incf (first (item-at vertexes id1)) (weight e))
                (incf (first (item-at vertexes id2)) (weight e)))
               (t
                (incf (second (item-at vertexes id1)) (weight e))
                (incf (second (item-at vertexes id2)) (weight e)))))))
    (sort 
     (collect-key-value
      vertexes
      :transform (lambda (k v) (list k (first v) (second v))))
     #'string-lessp
     :key #'first)))

;;; ---------------------------------------------------------------------------

(defclass* weighted-sampler-with-lookup-container ()
  ((sampler nil r)
   (lookup nil r)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object weighted-sampler-with-lookup-container)
                                       &key random-number-generator key)
  (setf (slot-value object 'sampler)
        (make-container 'weighted-sampling-container 
                        :random-number-generator random-number-generator
                        :key key)
        (slot-value object 'lookup)
        (make-container 'simple-associative-container)))

;;; ---------------------------------------------------------------------------

(defmethod insert-item ((container weighted-sampler-with-lookup-container)
                        (item t))
  (let ((node (nth-value 1 (insert-item (sampler container) item))))
    ;;?? remove
    (assert (not (null node)))
    (setf (item-at-1 (lookup container) item) node)))

;;; ---------------------------------------------------------------------------

(defmethod find-node ((container weighted-sampler-with-lookup-container)
                      (item t))
  (item-at-1 (lookup container) item))

;;; ---------------------------------------------------------------------------

(defmethod delete-node ((container weighted-sampler-with-lookup-container)
                        (node t))
  ;; not going to worry about the hash table
  (delete-node (sampler container) node))

;;; ---------------------------------------------------------------------------

(defmethod next-element ((container weighted-sampler-with-lookup-container))
  (next-element (sampler container)))

;;; ---------------------------------------------------------------------------

(defmethod generate-scale-free-graph
           (generator graph size kind-matrix add-edge-count
                      other-vertex-kind-samplers
                      vertex-creator
                      &key (duplicate-edge-function 'identity))
  (let* ((kind-count (array-dimension kind-matrix 0))
         (vertex-kinds (sample-vertexes-for-mixed-graph generator size kind-matrix))
         (vertex-sampler (make-array kind-count)))
    (save-generation-information graph generator 'generate-scale-free-graph)
    (flet ((sample-existing-vertexes (for-kind)
             ;; return list of vertexes to attach based on preferential attachment
             (loop for other-kind in (funcall (nth for-kind other-vertex-kind-samplers)
                                              add-edge-count generator) collect
                   (let ((vertex (next-element (aref vertex-sampler other-kind))))
                     (unless vertex
                       (loop for i from 0 
                             for nil across vertex-sampler 
                             until vertex do
                             (setf vertex (next-element (aref vertex-sampler i))
                                   other-kind i)))
                     
                     ;;?? remove. this should never happen
                     (unless vertex (break))
                     
                     (list vertex other-kind))))
           (update (kind thing)
             ;; handle bookkeeping for changed vertex degree
             (bind ((sampler (aref vertex-sampler kind))
                    (node (find-node sampler thing)))
               (delete-node sampler node)
               (insert-item sampler thing))))

      ;; set up samplers
      (loop for i from 0 
            for nil across vertex-sampler do
            (setf (aref vertex-sampler i)
                  (make-container 'weighted-sampler-with-lookup-container
                                  :random-number-generator generator
                                  :key (lambda (vertex)
                                         (1+ (vertex-degree vertex))))))
      
      ;; add vertexes and edges
      (loop for kind in (shuffle-elements! vertex-kinds :generator generator) 
            for i from 0 do
            (let* ((element (funcall vertex-creator kind i))
                   (vertex (add-vertex graph element)))
              (when (> i add-edge-count)
                (loop for (other other-kind) in (sample-existing-vertexes kind) do
                      (update other-kind other)
                      ;;?? remove
                      (if (or (null kind) (null other)) (break))
                      (add-edge-between-vertexes
                       graph vertex other
                       :if-duplicate-do 
                       (lambda (e) (funcall duplicate-edge-function e)))))
              (insert-item (aref vertex-sampler kind) vertex)))
      
      graph)))

;;; ---------------------------------------------------------------------------

#+Test
(defun poisson-connector (count generator)
  (let* ((ts (poisson-random generator 2))
         (cs (poisson-random generator 2))
         (rest (- count ts cs)))
    (loop for tick = t then (not tick) while (minusp rest) do
          (incf rest)
          (if tick (decf ts) (decf cs)))
    (shuffle-elements!
     (append (make-list (truncate rest) :initial-element 0)
             (make-list (truncate ts) :initial-element 1)
             (make-list (truncate cs) :initial-element 2))
     :generator generator)))

#+Test
(setf (ds :g-1100)
      (generate-scale-free-graph
       *random-generator*
       (make-container 'graph-container :default-edge-type :undirected)
       1100
       #(1000 50 50)
       10
       (list
        (lambda (count generator)
          (declare (ignore generator))
          (make-list count :initial-element 0))
        #'poisson-connector
        #'poisson-connector)
       (lambda (kind count) 
         (form-keyword (aref "BTC" kind) (format nil "~4,'0D" count)))))

#+Test
(pro:with-profiling
  (generate-scale-free-graph 
   *random-generator*
   (make-container 'graph-container :default-edge-type :undirected)
   10000
   #(1.0)
   10
   (list
    (lambda (count generator)
      (declare (ignore generator))
      (make-list count :initial-element 0)))
   (lambda (kind count) 
     (form-keyword (aref "BTC" kind) (format nil "~4,'0D" count)))))

#|
(pro:with-profiling
  (generate-scale-free-graph 
   *random-generator*
   (make-container 'graph-container :default-edge-type :undirected)
   1000
   #(1.0)
   3
   (list
    (lambda (count generator)
      (declare (ignore generator))
      (make-list count :initial-element 0)))
   (lambda (kind count) 
     (form-keyword (aref "BTC" kind) (format nil "~4,'0D" count)))))


;;; 61.4640 cpu seconds (61.4640 cpu seconds ignoring GC)
;;; 102,959,032 words consed
Execution time profile from 2078 samples
  Parents
Function
  Children                                   Relative  Absolute Consing       Conses
----
%%check-keywords                                            99%     99%  100,970,656
  sample-existing-vertexes                       62%
  insert-item <weighted-sampler-with-lookup-container> <t>  32%
  add-vertex <basic-graph> <t>                    2%
  update                                          1%
  add-edge-between-vertexes <basic-graph> <basic-vertex> <basic-vertex>   1%
  form-keyword                                    1%
  iterate-container <contents-as-array-mixin> <t>   1%
----
  %%check-keywords                              100%
sample-existing-vertexes                                    62%     61%   62,577,336
  walk-tree-nodes <bst-node> <t>                 99%
  uniform-random                                  1%
----
  sample-existing-vertexes                      100%
walk-tree-nodes <bst-node> <t>                              61%     60%   61,607,072
  #<anonymous function #xaa2070e>                77%
  +-2                                             3%
  element-weight <weighted-sampling-container> <t>   2%
  >=-2                                            2%
  %double-float+-2!                               1%
  %%one-arg-dcode                                 1%
----
  walk-tree-nodes <bst-node> <t>                 98%
  %%before-and-after-combined-method-dcode        2%
#<anonymous function #xaa2070e>                             48%     47%   48,156,256
  iterate-container <contents-as-array-mixin> <t>  73%
  %%1st-two-arg-dcode                             9%
  iterate-edges <graph-container-vertex> <t>      6%
  constantly                                      4%
  iterate-elements <abstract-container> <t>       2%
----
  #<anonymous function #xaa2070e>                99%
  %vertex-degree                                  1%
iterate-container <contents-as-array-mixin> <t>             35%     35%   35,440,856
  other-vertex <graph-container-edge> <graph-container-vertex>  43%
  %%nth-arg-dcode                                20%
  #<anonymous function #x271d31e>                10%
----
  insert-item <weighted-sampler-with-lookup-container> <t>  92%
  %make-std-instance                              3%
  update                                          3%
  %%standard-combined-method-dcode                1%
  %call-next-method                               1%
%%before-and-after-combined-method-dcode                    34%     34%   34,400,720
  insert-item <binary-search-tree> <bst-node>    90%
  #<anonymous function #xaa2070e>                 2%
  shared-initialize <standard-object> <t>         2%
  %%one-arg-dcode                                 1%
  %double-float+-2!                               1%
  +-2                                             1%
----
  %%check-keywords                              100%
insert-item <weighted-sampler-with-lookup-container> <t>    31%     31%   31,970,488
  %%before-and-after-combined-method-dcode      100%
----
  %%before-and-after-combined-method-dcode      100%
insert-item <binary-search-tree> <bst-node>                 30%     31%   31,227,120
  %vertex-degree                                 84%
  vertex-degree                                   5%
----
  insert-item <binary-search-tree> <bst-node>    99%
  #<anonymous function #xaa2070e>                 1%
%vertex-degree                                              26%     25%   25,870,312
  #<anonymous function #xa7cee86>                68%
  %aref1                                          3%
  %std-slot-value-using-class                     1%
  slot-id-value                                   1%
  %%one-arg-dcode                                 1%
  iterate-container <contents-as-array-mixin> <t>   1%
----
  %vertex-degree                                 99%
  iterate-container <contents-as-array-mixin> <t>   1%
#<anonymous function #xa7cee86>                             18%     17%   17,420,592
  %maybe-std-slot-value-using-class               8%
  %%one-arg-dcode                                 8%
  %std-slot-value-using-class                     8%
  slot-id-value                                   5%
  vertex-1 <graph-container-edge>                 5%
  #<anonymous function #x271d31e>                 1%
----
  iterate-container <contents-as-array-mixin> <t>  99%
  #<anonymous function #xa7cee86>                 1%
other-vertex <graph-container-edge> <graph-container-vertex>   15%     14%   14,029,496
  %%one-arg-dcode                                 1%
----
  iterate-container <contents-as-array-mixin> <t>  95%
  %%check-keywords                                3%
  %%before-and-after-combined-method-dcode        1%
  initialize-instance (around) <basic-initial-contents-mixin>   1%
%%nth-arg-dcode                                              7%      9%    9,238,560
----
  #<anonymous function #xaa2070e>                93%
  walk-tree-nodes <bst-node> <t>                  5%
  %%before-and-after-combined-method-dcode        2%
%%1st-two-arg-dcode                                          5%      5%    4,802,264
----
  iterate-container <contents-as-array-mixin> <t>  96%
  #<anonymous function #xa7cee86>                 3%
  shared-initialize <standard-object> <t>         1%
#<anonymous function #x271d31e>                              4%      4%    4,012,368
----
  #<anonymous function #xaa2070e>               100%
iterate-edges <graph-container-vertex> <t>                   3%      3%    2,918,352
----
  #<anonymous function #xa7cee86>                59%
  %vertex-degree                                 14%
  walk-tree-nodes <bst-node> <t>                 13%
  shared-initialize <standard-object> <t>         6%
  %shared-initialize                              4%
  other-vertex <graph-container-edge> <graph-container-vertex>   2%
  member                                          2%
%std-slot-value-using-class                                  2%      2%    2,115,320
----
  #<anonymous function #xa7cee86>                59%
  walk-tree-nodes <bst-node> <t>                 12%
  %vertex-degree                                  9%
  %%before-and-after-combined-method-dcode        6%
  shared-initialize <standard-object> <t>         4%
  update                                          4%
  other-vertex <graph-container-edge> <graph-container-vertex>   4%
  %shared-initialize                              2%
%%one-arg-dcode                                              2%      2%    2,478,304
----
  make-instance <symbol>                         68%
  %make-instance                                 23%
  make-instance <standard-class>                  9%
%make-std-instance                                           2%      2%    2,283,344
  %%before-and-after-combined-method-dcode       47%
  shared-initialize <standard-object> <t>        15%
  %%standard-combined-method-dcode               12%
  %maybe-std-slot-value-using-class               3%
----
  #<anonymous function #xa7cee86>                78%
  %vertex-degree                                  7%
  uniform-random                                  5%
  %make-std-instance                              2%
  shared-initialize <standard-object> <t>         3%
  view-get <simple-view> <t>                      2%
  walk-tree-nodes <bst-node> <t>                  3%
%maybe-std-slot-value-using-class                            2%      2%    2,005,048
----
  add-edge-between-vertexes <basic-graph> <basic-vertex> <basic-vertex>  42%
  add-vertex <basic-graph> <t>                   40%
  initialize-instance (after) <graph-container-vertex>   7%
  add-it                                          6%
  %%before-and-after-combined-method-dcode        5%
make-instance <symbol>                                       2%      2%    1,932,504
  %make-std-instance                             92%
----
  #<anonymous function #xaa2070e>               100%
constantly                                                   2%      2%    1,629,880
----
  walk-tree-nodes <bst-node> <t>                 97%
  %%before-and-after-combined-method-dcode        3%
+-2                                                          2%      2%    1,688,392
  %maybe-std-slot-value-using-class               3%
----
  %%check-keywords                              100%
add-vertex <basic-graph> <t>                                 2%      2%    2,259,304
  make-instance <symbol>                         44%
  %%standard-combined-method-dcode               30%
  %%before-and-after-combined-method-dcode        8%
  %make-std-instance                              3%
----
generate-scale-free-graph <t> <t> <t> <t> <t> <t> <t>        2%      2%    1,700,920
  %%standard-combined-method-dcode               48%
  %%check-keywords                               16%
  uniform-random                                 15%
  make-instance <symbol>                          6%
----
  generate-scale-free-graph <t> <t> <t> <t> <t> <t> <t>  45%
  add-vertex <basic-graph> <t>                   25%
  %make-std-instance                             18%
  make-instance <standard-class>                  6%
  add-it                                          3%
  insert-item <weighted-sampler-with-lookup-container> <t>   3%
%%standard-combined-method-dcode                             2%      2%    2,019,832
  insert-item <container-uses-nodes-mixin> <t>   45%
  %%before-and-after-combined-method-dcode       25%
  %%nth-arg-dcode                                 3%
  make-instance <symbol>                          3%
----
#<GRAPH-CONTAINER 1000>
? 2
2
? 

(open-plot-in-window
 (histogram 
  (collect-elements
   (clnuplot::data->n-buckets
    (sort (collect-items x :transform #'vertex-degree) #'>)
    20
    #'identity)
   :filter 
   (lambda (x)
     (and (plusp (first x))
          (plusp (second x ))))
   :transform 
   (lambda (x)
     (list (log (first x) 10) (log (second x)))))))



(clasp:linear-regression-brief 
 (mapcar #'first 
         '((2.3453737305590883 6.812345094177479) (2.819872821950546 3.871201010907891)
          (3.041195233696809 2.6390573296152584) (3.1870975005834774 2.1972245773362196)
          (3.2961164921697144 1.6094379124341003)
          (3.3831867994748994 1.9459101490553132)
          (3.4556821645007902 0.6931471805599453)
          (3.5721161556642747 1.3862943611198906) (3.909743184806193 0.0)
          (3.932600584500482 0.0))
         )
 (mapcar #'second 
         '((2.3453737305590883 6.812345094177479) (2.819872821950546 3.871201010907891)
          (3.041195233696809 2.6390573296152584) (3.1870975005834774 2.1972245773362196)
          (3.2961164921697144 1.6094379124341003)
          (3.3831867994748994 1.9459101490553132)
          (3.4556821645007902 0.6931471805599453)
          (3.5721161556642747 1.3862943611198906) (3.909743184806193 0.0)
          (3.932600584500482 0.0))
         ))

|#

;;; ---------------------------------------------------------------------------
;;; generate-assortative-graph-with-degree-distributions
;;; ---------------------------------------------------------------------------

#+Ignore
(define-debugging-class generate-assortative-graph-with-degree-distributions ())

;;; ---------------------------------------------------------------------------

(defmethod generate-assortative-graph-with-degree-distributions
           (generator (graph-class symbol)
                      edge-count assortativity-matrix
                      average-degrees
                      degree-distributions
                      vertex-creator
                      &key (duplicate-edge-function 'identity)) 
  (generate-assortative-graph-with-degree-distributions
   generator (make-instance graph-class) 
   edge-count assortativity-matrix
   average-degrees
   degree-distributions
   vertex-creator
   :duplicate-edge-function duplicate-edge-function))

#|
Split into a function to compute some of the intermediate pieces and one to use them
|#

(defmethod generate-assortative-graph-with-degree-distributions
           (generator graph edge-count assortativity-matrix
                      average-degrees
                      degree-distributions
                      vertex-creator
                      &key (duplicate-edge-function 'identity)) 
  (setf assortativity-matrix (normalize-matrix assortativity-matrix))
  (let* ((kind-count (array-dimension assortativity-matrix 0))
         (vertex->degree-counts (make-array kind-count))
         (edges (copy-tree
                 (sample-edges-for-assortative-graph 
                  generator edge-count assortativity-matrix)))
         (degree-sums (sort
                       (merge-elements 
                        (append (element-counts edges :key #'first)
                                (element-counts edges :key #'second))
                        (lambda (old new)
                          (+ old new))
                        (lambda (new)
                          new) :key #'first :argument #'second)
                       #'<
                       :key #'first))
         (vertex-counts (collect-elements 
                         degree-sums
                         :transform 
                         (lambda (kind-and-count)
                           (round (float (/ (second kind-and-count)
                                            (elt average-degrees (first kind-and-count))))))))
         (edge-samplers (make-array kind-count)))
    (save-generation-information graph generator 'generate-assortative-graph-with-degree-distributions)
    
    ;; setup bookkeeping
    (loop for kind from 0 to (1- kind-count) do
          (setf (aref edge-samplers kind) 
                (make-container 'vector-container)
                (aref vertex->degree-counts kind)
                (make-container 'simple-associative-container)))
    (loop for edge in edges do
          (insert-item (aref edge-samplers (first edge)) (cons :source edge))
          (insert-item (aref edge-samplers (second edge)) (cons :target edge)))
    (iterate-elements
     edge-samplers (lambda (sampler) (shuffle-elements! sampler :generator generator)))

    ;(spy edges degree-sums vertex-counts)

    (loop for kind from 0 to (1- kind-count)
          for count in vertex-counts do
          (let ((distribution (nth-element degree-distributions kind))
                (vertexes (make-container 'vector-container))
                (vertex-degrees (aref vertex->degree-counts kind))
                (total-degree 0)
                (desired-sum (second (elt degree-sums kind)))) 
            
            ;; for each type, create vertexes
            (loop for i from 0 to (1- count) do
                  (let ((vertex (funcall vertex-creator kind i))
                        (degree (funcall distribution)))
                    (insert-item vertexes vertex)
                    (setf (item-at-1 vertex-degrees vertex)
                          degree)
                    (incf total-degree degree)))
            
            ;(spy vertexes total-degree desired-sum) 
            
            ;; ensure proper total degree
            (loop while (/= total-degree desired-sum) do
                  #+Ignore
                  (when-debugging-format
                   generate-assortative-graph-with-degree-distributions
                   "Current: ~D, Desired: ~D, Difference: ~D" 
                   total-degree desired-sum
                   (abs (- total-degree desired-sum)))
                  (let* ((vertex (sample-element vertexes generator))
                         (bigger? (< total-degree desired-sum))
                         (current-degree (item-at-1 vertex-degrees vertex))
                         (new-degree 0)
                         (attempts 100))
                    (when (or bigger?
                              (and (not bigger?) 
                                   (plusp current-degree)))
                      (decf total-degree current-degree)
                      
                      #+Ignore
                      (when-debugging-format
                       generate-assortative-graph-with-degree-distributions
                       "  ~D ~D ~:[^~]"
                       total-degree current-degree new-degree (not bigger?))
                      
                      ;; increase speed by knowing which direction we need to go...?
                      (loop until (or (zerop (decf attempts)) 
                                      (and bigger? 
                                           (> (setf new-degree (funcall distribution))
                                              current-degree))
                                      (and (not bigger?)
                                           (< (setf new-degree (funcall distribution))
                                              current-degree))) do
                            
                            (setf bigger? (< (+ total-degree new-degree) desired-sum)))
                      
                      (cond ((plusp attempts)
                             #+Ignore
                             (when-debugging
                               generate-assortative-graph-with-degree-distributions
                               (format *debug-io* " -> ~D" new-degree))
                             
                             (setf (item-at-1 vertex-degrees vertex) new-degree)
                             (incf total-degree new-degree)

                             #+Ignore
                             (when-debugging-format
                              generate-assortative-graph-with-degree-distributions
                              "~D ~D" total-degree desired-sum))
                            (t
                             ;; couldn't find one, try again
                             (incf total-degree current-degree))))))
            
            ;; attach edges
            (let ((edge-sampler (aref edge-samplers kind)))
              (flet ((sample-edges-for-vertex (vertex)
                       ;(spy vertex)
                       (loop repeat (item-at-1 vertex-degrees vertex) do
                             (bind (((edge-kind . edge) (delete-last edge-sampler)))
                               (ecase edge-kind
                                 (:source (setf (first edge) vertex))
                                 (:target (setf (second edge) vertex)))))))
                (iterate-elements 
                 vertexes
                 #'sample-edges-for-vertex)))))
    
    ;; repair self edges
    
    
    ;; now make the graph [at last]
    (iterate-elements 
     edges
     (lambda (edge)
       (add-edge-between-vertexes graph (first edge) (second edge)
                                  :if-duplicate-do duplicate-edge-function))))
  
  graph)
    
#+Test
(generate-assortative-graph-with-degree-distributions 
 *random-generator*
 'graph-container
 100
 #2A((0.1111111111111111 0.2222222222222222)
    (0.2222222222222222 0.4444444444444444))
 #+No
 #2A((0.011840772766222637 0.04524421593830334)
     (0.04524421593830334 0.8976707953571706))
 '(3 3)
 (list 
  (make-degree-sampler
   (lambda (i)
     (poisson-vertex-degree-distribution 3 i))
   :generator *random-generator*)
  (make-degree-sampler
   (lambda (i)
     (poisson-vertex-degree-distribution 3 i))
   :generator *random-generator*))
 
 (lambda (kind count) 
   (form-keyword (aref "BTC" kind) (format nil "~4,'0D" count))))

#+Test
(element-counts
 (copy-tree
  (sample-edges-for-assortative-graph 
   *random-generator*
   100
   #2A((0.1111111111111111 0.2222222222222222)
       (0.2222222222222222 0.4444444444444444))))
 :test #'eq)

;;; ---------------------------------------------------------------------------
;;; generate-graph-by-resampling-edges
;;; ---------------------------------------------------------------------------

#|
doesn't take edge weights into account when sampling

should include pointer back to original graph
|#

(defclass* basic-edge-sampler ()
  ((generator nil ir)
   (graph nil ir)))

;;; ---------------------------------------------------------------------------

(defmethod next-element ((sampler basic-edge-sampler))
  (sample-element (graph-edges (graph sampler)) (generator sampler)))

;;; ---------------------------------------------------------------------------

(defclass* weighted-edge-sampler (basic-edge-sampler)
  ((weight-so-far 0 a)
   (index-iterator nil r)
   (edge-iterator nil r)
   (size nil ir)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object weighted-edge-sampler) &key)
  (bind ((generator (generator object))
         (weighted-edge-count 
          (let ((result 0))
            (iterate-edges (graph object) (lambda (e) (incf result (weight e))))
            result)))
    (unless (size object)
      (setf (slot-value object 'size) weighted-edge-count))   
    (setf (slot-value object 'index-iterator)
          (make-iterator
           (sort (loop repeat (size object) collect
                       (integer-random generator 1 weighted-edge-count)) #'<))
          (slot-value object 'edge-iterator) 
          (make-iterator (graph-edges (graph object))))))
       
;;; ---------------------------------------------------------------------------

(defmethod next-element ((object weighted-edge-sampler))
  (let ((edge-iterator (edge-iterator object))
        (index-iterator (index-iterator object)))
    (move-forward index-iterator)
    (loop while (< (weight-so-far object) (current-element index-iterator)) do
          (move-forward edge-iterator)
          (incf (weight-so-far object) (weight (current-element edge-iterator))))
    (current-element edge-iterator)))

;;; ---------------------------------------------------------------------------        

(defmethod generate-graph-by-resampling-edges 
           (generator original-graph &key
                      (edge-sampler-class 'basic-edge-sampler)
                      (edge-count (edge-count original-graph)))
  (let ((graph (copy-template original-graph))
        (edge-sampler (make-instance edge-sampler-class
                        :generator generator
                        :graph original-graph
                        :size edge-count)))
    (save-generation-information graph generator 'generate-graph-by-resampling-edges)
    
    ;; vertexes
    (iterate-vertexes
     original-graph
     (lambda (v)
       (add-vertex graph (element v))))
    
    ;; sample edges
    (loop repeat edge-count do
          (let ((edge (next-element edge-sampler)))
            (if (directed-edge-p edge)
              (add-edge-between-vertexes 
               graph (element (source-vertex edge)) (element (target-vertex edge))
               :edge-type :directed
               :if-duplicate-do (lambda (e) (incf (weight e))))
              (add-edge-between-vertexes 
               graph (element (vertex-1 edge)) (element (vertex-2 edge))
               :edge-type :undirected
               :if-duplicate-do (lambda (e) (incf (weight e)))))))
    
    graph))
              
#+Test
(fluid-bind (((random-seed *random-generator*) 1))
  (let* ((dd-1 (lambda (i)
                 #+Ignore
                 (power-law-vertex-degree-distribution 3 i)
                 (poisson-vertex-degree-distribution 3 i)))
         (dd-2 (lambda (i)
                 #+Ignore
                 (power-law-vertex-degree-distribution 3 i)
                 (poisson-vertex-degree-distribution 3 i)))
         (g (generate-assortative-graph-with-degree-distributions 
             *random-generator*
             (make-instance 'graph-container
               :default-edge-type :undirected
               :undirected-edge-class 'weighted-edge)
             100
             #2A((0.011840772766222637 0.04524421593830334)
                 (0.04524421593830334 0.8976707953571706))
             '(3 3)
             (list 
              (make-degree-sampler
               dd-1
               :generator *random-generator*
               :max-degree 40
               :min-probability nil)
              (make-degree-sampler
               dd-2
               :generator *random-generator*
               :max-degree 40
               :min-probability nil))
             #'simple-group-id-generator
             :duplicate-edge-function (lambda (e) (incf (weight e))))))
    (flet ((avd (g)
             (average-vertex-degree 
              g
              :vertex-filter (lambda (v)
                               (plusp (edge-count v)))
              :edge-size #'weight)))
      (print (avd g))
      (loop for i from 1 to 10
            do
            (fluid-bind (((random-seed *random-generator*) i))
              (print (avd
                      (generate-graph-by-resampling-edges
                       *random-generator* g 'weighted-edge-sampler (edge-count g)))))))))

;;; ---------------------------------------------------------------------------
;;; some preferential attachment algorithms 
;;; ---------------------------------------------------------------------------

#+Ignore
(define-debugging-class generate-preferential-attachment-graph
  (graph-generation))

;;; ---------------------------------------------------------------------------

(defmethod generate-simple-preferential-attachment-graph
           (generator graph size minimum-degree)
  (bind ((m (make-array (list (* 2 size minimum-degree)))))
    (loop for v from 0 to (1- size) do
          (loop for i from 0 to (1- minimum-degree) do
                (bind ((index (* 2 (+ i (* v minimum-degree))))
                       (r (integer-random generator 0 index)))
                  (setf (item-at m index) v
                        (item-at m (1+ index)) (item-at m r)))))
    (loop for i from 0 to (1- (* size minimum-degree)) do
          (add-edge-between-vertexes 
           graph (item-at m (* 2 i)) (item-at m (1+ (* 2 i)))))
    graph))

#+Test
(setf (ds :g-b)
      (generate-simple-preferential-attachment-graph
       *random-generator*
       (make-container 'graph-container :default-edge-type :undirected)
       10000
       10))

#+Test
(element-counts 
   (collect-nodes (ds :g-b)
                  :transform (lambda (v) (list (element v) (vertex-degree v))))
   :key #'second
   :sort #'>
   :sort-on :values)

;;; ---------------------------------------------------------------------------

(defmethod generate-preferential-attachment-graph
           (generator (graph-class symbol) size kind-matrix minimum-degree 
                      assortativity-matrix 
                      &key (vertex-labeler 'simple-group-id-generator) 
                      (duplicate-edge-function :ignore)) 
  (generate-preferential-attachment-graph
   generator (make-instance graph-class)
   size kind-matrix minimum-degree assortativity-matrix
   :vertex-labeler vertex-labeler
   :duplicate-edge-function duplicate-edge-function))

;;; ---------------------------------------------------------------------------
  
(defmethod generate-preferential-attachment-graph
           (generator (graph basic-graph) size kind-matrix minimum-degree 
                      assortativity-matrix 
                      &key (vertex-labeler 'simple-group-id-generator) 
                      (duplicate-edge-function :ignore))
  (bind ((kind-count (array-dimension kind-matrix 0))
         (vertex-kinds (sample-vertexes-for-mixed-graph generator size kind-matrix))
         (vertex-kind-counts (element-counts vertex-kinds :sort #'< :sort-on :values))
         (edge-recorders (make-array (list kind-count)))
         (count-recorders (make-array (list kind-count) :initial-element 0))
         (edge-samplers (make-array (list kind-count))))
    
    ;; set up record keeping
    (dotimes (i kind-count)
      (setf (aref edge-recorders i) 
            (make-array (list (* 2 (item-at vertex-kind-counts i) minimum-degree))
                        :initial-element nil))
      (setf (aref edge-samplers i) 
            (make-edge-sampler-for-preferential-attachment-graph
             generator (array-row assortativity-matrix i))))

    ;; add vertexes (to ensure that we have something at which to point)
    (loop for v from 0 to (1- size)
          for kind in vertex-kinds do
          (bind ((edge-recorder (aref edge-recorders kind)))
            (loop for i from 0 to (1- minimum-degree) do
                  (bind ((index (* 2 (+ i (* (aref count-recorders kind) minimum-degree)))))
                    (setf (item-at edge-recorder index) 
                          (funcall vertex-labeler kind v)))))
          (incf (aref count-recorders kind)))
    
    ;; determine edges
    (dotimes (i kind-count)
      (setf (aref count-recorders i) 0))
    (loop for v from 0 to (1- size)
          for kind in vertex-kinds do
          (bind ((edge-recorder (aref edge-recorders kind))
                 (edge-sampler (aref edge-samplers kind)))
            (loop for i from 0 to (1- minimum-degree) do
                  (bind ((index (* 2 (+ i (* (aref count-recorders kind) minimum-degree))))
                         (other-kind (funcall edge-sampler)) 
                         (other-index (* 2 (+ i (* (min (1- (item-at vertex-kind-counts other-kind))
                                                        (aref count-recorders other-kind))
                                                   minimum-degree))))
                         (other-edge-recorder (aref edge-recorders other-kind))
                         (r (integer-random generator 0 (1- other-index))))
                    #+Ignore
                    (when-debugging-format 
                     generate-preferential-attachment-graph
                     "[~2D ~6D] [~2D ~6D] (max: ~6D)"
                     kind (1+ index) other-kind r other-index) 
                    (setf (item-at edge-recorder (1+ index)) 
                          (cond ((item-at other-edge-recorder r)
                                 (item-at other-edge-recorder r))
                                ((and (= kind other-kind)
                                      (= (1+ index) r))
                                 ;; it's me!
                                 (item-at edge-recorder index))
                                (t
                                 ;; haven't done the other one yet... save it for later fixing
                                 (list other-kind r))))))
            (incf (aref count-recorders kind))))
    
    ;; record fixups
    (let ((corrections 0)
          (last-corrections nil)
          (again? t))
      (loop while again? do
            (setf corrections 0
                  again? nil)
            (dotimes (kind kind-count)
              (loop for vertex across (aref edge-recorders kind)
                    for index = 0 then (1+ index) 
                    when (consp vertex) do
                    (bind (((other-kind other-index) vertex))
                      #+Ignore
                      (when-debugging-format 
                       generate-preferential-attachment-graph "~2D ~10D, ~A -> ~A" 
                       kind index vertex
                       (aref (aref edge-recorders other-kind) other-index))
                      (incf corrections)
                      (if (and (= kind other-kind) (= index other-index))
                        ;; pointing at myself
                        (setf (aref (aref edge-recorders kind) index) 
                              (aref (aref edge-recorders kind) (1- index)))
                        (let ((new (aref (aref edge-recorders other-kind) other-index)))
                          (when (consp new)
                            (setf again? t))
                          (setf (aref (aref edge-recorders kind) index) new))))))
            (when (and last-corrections 
                       (>= corrections last-corrections))
              (error "It's not getting any better old boy"))
            (setf last-corrections corrections)))
    
    ;; make sure we got 'em all
    (dotimes (i kind-count)
      (loop for vertex across (aref edge-recorders i) 
            when (not (symbolp vertex)) do (error "bad function, down boy")))

    (dotimes (i kind-count)
      (let ((edge-recorder (aref edge-recorders i)))
        (loop for index from 0 to (1- (size edge-recorder)) by 2 do 
              (add-edge-between-vertexes 
               graph (item-at edge-recorder index) (item-at edge-recorder (1+ index))
               :if-duplicate-do duplicate-edge-function))))
    
    #|
;; record properties
    (record-graph-properties graph)
    (setf (get-value graph :initial-seed) (random-seed generator))
    (setf (get-value graph :size) size
          (get-value graph :minimum-degree) minimum-degree
          (get-value graph :assortativity-matrix) assortativity-matrix
          (get-value graph :duplicate-edge-function) duplicate-edge-function)
|#
    
    graph))

;;; ---------------------------------------------------------------------------

(defun make-edge-sampler-for-preferential-attachment-graph (generator assortativities)
  (let ((c (make-container 'weighted-sampling-container
                           :random-number-generator generator
                           :key (lambda (item)
                                  (aref assortativities item)))))
    (dotimes (i (array-dimension assortativities 0))
      (insert-item c i))
    (lambda () (next-element c))))


#+Test
(let ((s
       (make-edge-sampler-for-preferential-attachment-graph 
        *random-generator* #(0.02 0.25 0.25))))
  (loop repeat 100 collect (funcall s)))

#+Test
(progn
  (setf (random-seed *random-generator*) 2)
  (generate-preferential-attachment-graph
   *random-generator*
   (make-graph 'graph-container :edge-type :undirected)
   100
   #(90 5 5)
   3
   #2A((0.96 0.02 0.02)
       (0.02 0.25 0.25)
       (0.02 0.25 0.25))))

#+Test
(generate-preferential-attachment-graph
 *random-generator*
 (make-graph 'graph-container :edge-type :undirected)
 1100
 #(1000 50 50)
 3
 #2A((0.96 0.02 0.02)
     (0.02 0.25 0.25)
     (0.02 0.25 0.25)))

#+Test
(pro:with-profiling
  (generate-preferential-attachment-graph
   *random-generator*
   (make-graph 'graph-container :edge-type :undirected)
   11000
   #(10000 500 500)
   3
   #2A((0.96 0.02 0.02)
       (0.02 0.25 0.25)
       (0.02 0.25 0.25))))

;;; ---------------------------------------------------------------------------

(Defmethod generate-acquaintance-network 
           (generator graph size death-probability iterations vertex-labeler
                      &key (duplicate-edge-function :ignore))
  ;; bring the graph up to size
  (loop for i from (size graph) to (1- size) do
        (add-vertex graph (funcall vertex-labeler 0 i)))
  
  (loop repeat iterations do 
        (add-acquaintance-and-maybe-kill-something 
         generator graph death-probability duplicate-edge-function)) 
  (values graph))

;;; ---------------------------------------------------------------------------

(defmethod generate-acquaintance-network-until-stable 
           (generator graph size death-probability step-count 
                      stability-fn vertex-labeler
                      &key (duplicate-edge-function :ignore))
  ;; bring the graph up to size
  (loop for i from (size graph) to (1- size) do
        (add-vertex graph (funcall vertex-labeler 0 i)))
  
  (loop do
        (loop repeat step-count do
              (add-acquaintance-and-maybe-kill-something 
               generator graph death-probability duplicate-edge-function))
        (when (funcall stability-fn graph)
          (return)))
  
  (values graph))

;;; ---------------------------------------------------------------------------

(defun add-acquaintance-and-maybe-kill-something 
       (generator graph death-probability duplicate-edge-function)
  ;; add edges step 
  (bind ((vertex (sample-element (graph-vertexes graph) generator))
         (neighbors (when (>= (size (vertex-edges vertex)) 2)
                      (sample-unique-elements 
                       (vertex-edges vertex) generator 2))))
    (flet ((sample-other-vertex ()
             (loop for result = (sample-element (graph-vertexes graph) generator)
                   until (not (eq vertex result))
                   finally (return result))))  ;; CTM: 'finally do' not legal in openMCL
      (if neighbors
        (add-edge-between-vertexes 
         graph
         (other-vertex (first neighbors) vertex)
         (other-vertex (second neighbors) vertex)
         :if-duplicate-do duplicate-edge-function)
        (add-edge-between-vertexes 
         graph vertex (sample-other-vertex)
         :if-duplicate-do duplicate-edge-function))))
  
  ;; remove vertexes step
  (when (random-boolean generator death-probability)
    (let ((vertex (sample-element (graph-vertexes graph) generator)))
      (delete-vertex graph vertex)
      (add-vertex graph (element vertex)))))
  
#+Test
(generate-acquaintance-network
 *random-generator*
 (make-graph 'graph-container :edge-type :undirected)
 1000
 0.001 
 10000
 'simple-group-id-generator)