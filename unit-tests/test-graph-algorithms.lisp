(in-package #:cl-graph-test)

(deftestsuite test-connected-components ()
  ())

(addtest (test-connected-components)
  test-1
  (let ((g (make-container 'graph-container  :default-edge-type :undirected)))
    (loop for v in '(a b c d e f g h i j) do (add-vertex g v))
    (loop for (v1 v2) in '((a b) (a c) ( b c) (b d) (e f) (e g) (h i)) do
          (add-edge-between-vertexes g v1 v2))
    
    (let ((cc (connected-components g)))
      (flet ((test (a b result)
               (ensure-same (eq (representative-node cc (find-vertex g a))
                                (representative-node cc (find-vertex g b)))
                            result)))
        (loop for (v1 v2 result) in '((a b t) (a e nil) (f g t)
                               (j c nil) (b a t) (d c t)) do
              (test v1 v2 result))))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-minimum-spanning-tree ()
  ())

(deftestsuite test-mst-kruskal (test-minimum-spanning-tree)
  ())

(addtest (test-mst-kruskal)
  test-1
  (let ((g (make-container 'graph-container
                           :default-edge-type :undirected
                           :undirected-edge-class 'weighted-edge))
        (m nil))
    (loop for (v1 v2 w) in '((a b 4) (a h 9)
                             (b c 8) (b h 11)
                             (c i 2) (c d 7) (c f 4)
                             (d e 9) (d f 14)
                             (e f 10) 
                             (f g 2)
                             (g h 1) (g i 6)
                             (h i 7)) do
          (add-edge-between-vertexes g v1 v2 :weight w))
    (setf m (minimum-spanning-tree-kruskal g))
    (ensure (set-equal 
             '(a b c d e f g h i)
             (flatten (mapcar (lambda (e)
                                (list (element (vertex-1 e)) (element (vertex-2 e)))) 
                              m))))
    (ensure-same (reduce #'+ m :key 'weight) 37 :test '=)
    (ensure-same (size m) 8)))

#+Test
(defclass* directed-weighted-edge (weighted-edge-mixin graph-container-directed-edge)
  ())

#+Test
(let ((g (make-container 'graph-container
                         :default-edge-type :undirected
                         :undirected-edge-class 'weighted-edge
                         :directed-edge-class 'directed-weighted-edge)))
  (loop for (v1 v2 w) in '((a b 4) (a h 9)
                           (b c 8) (b h 11)
                           (c i 2) (c d 7) (c f 4)
                           (d e 9) (d f 14)
                           (e f 10) 
                           (f g 2)
                           (g h 1) (g i 6)
                           (h i 7)
                           
                           (a h 3)) do
        (add-edge-between-vertexes g v1 v2 :weight w 
                                   :edge-type (if (random-boolean *random-generator* 0.3) 
                                                :directed :undirected)))
  (minimum-spanning-tree-kruskal g))
        
#+Test
(graph->dot 
 (let ((g (make-container 'graph-container
                          :default-edge-type :undirected
                          :undirected-edge-class 'weighted-edge))
       (m nil))
   (loop for (v1 v2 w) in '((a b 10) (a b 1) (a d 3)
                           (b c 1) (b d 3)
                           (c d 1)) do
         (add-edge-between-vertexes g v1 v2 :weight w :if-duplicate-do :force))
   (setf m (minimum-spanning-tree-kruskal g))
   g)
 "p2dis:data;x.dot")

#+Test
(let ((g (make-container 'graph-container
                         :default-edge-type :undirected
                         :undirected-edge-class 'weighted-edge))
      (m nil))
  (loop for (v1 v2 w) in '((a b 1) (a d 3)
                           (b c 5) (b d 2)
                           (c d 1)) do
        (add-edge-between-vertexes g v1 v2 :weight w :if-duplicate-do :force))
  (setf m (minimum-spanning-tree-kruskal g))
  m)

;;; ---------------------------------------------------------------------------

#+test
(let ((graph (make-container 'graph-container)))
  (loop for (a b) in '((r s) (r v) (s w) (t u) (t w) (t x) 
                       (u y) (w x) (x y)) do
        (add-edge-between-vertexes graph a b))
  
  (breadth-first-search-graph graph 's))

;;; ---------------------------------------------------------------------------

#+Ignore
(let ((graph (make-container 'graph-container)))
  (loop for (a b) in '((r s) (r v) (s w) (t u) (t w) (t x) 
                       (u y) (w x) (x y)) do
        (add-edge-between-vertexes graph a b))
  
  (breadth-first-visitor graph 's #'print))