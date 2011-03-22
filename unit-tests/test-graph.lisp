(in-package #:cl-graph-test)

#|
(let ((g (make-container 'graph-container))) 
  (add-edge-between-vertexes g 'a 'b)
  (let ((v-a (find-vertex g 'a))
        (v-b (find-vertex g 'b)))
    (print (compute-applicable-methods #'(SETF ELEMENT) (list :NEW-A V-A)))
    (setf (element v-a) :new-a)
    (inspect g)))
|#

(deftestsuite cl-graph-test () ())

(deftestsuite test-test-vertex (cl-graph-test) ())

(addtest (test-test-vertex)
  test-1
  (let ((x (float 2.1d0))
	(y (float 2.1d0))
	(g (make-container 'graph-container)))
    (add-vertex g (+ x y))
    (add-vertex g (+ x y))
    
    (ensure-same (size g) 2)))

(addtest (test-test-vertex)
  test-2
  (let ((x (float 2.1d0))
         (y (float 2.1d0))
         (g (make-container 'graph-container :vertex-test #'=)))
    (add-vertex g (+ x y))
    (add-vertex g (+ x y))
    
    (ensure-same (size g) 1)))


;;; ---------------------------------------------------------------------------
;;; should do this for each _kind_ of graph
;;; ---------------------------------------------------------------------------

(deftestsuite test-basic-graph-properties (cl-graph-test)
  ((graph-undirected (make-container 'graph-container
				     :default-edge-type :undirected))
   (graph-directed (make-container 'graph-container
				   :default-edge-type :directed)))
  :setup ((loop for v in '(a b c d e) do
                (add-vertex graph-undirected v)
                (add-vertex graph-directed v))
          (loop for (v1 . v2) in '((a . b) (a . c) (b . d) (c . e)) do
                (add-edge-between-vertexes graph-undirected v1 v2)
                (add-edge-between-vertexes graph-directed v1 v2))))

#+(or)
(let ((g (make-container 'graph-container
			 :default-edge-type :directed)))
  (loop for v in '(a b c d e) do
                (add-vertex g v))
  (loop for (v1 . v2) in '((a . b) (a . c) (b . d) (c . e)) do
       (add-edge-between-vertexes g v1 v2))
  g)

;;; ---------------------------------------------------------------------------

(addtest (test-basic-graph-properties)
  (ensure-same (size (graph-vertexes graph-directed)) 5 :test #'=)
  (ensure-same (size (graph-edges graph-directed)) 4 :test #'=))

;;; ---------------------------------------------------------------------------

(addtest (test-basic-graph-properties)
  (delete-edge-between-vertexes graph-directed 'a 'b)
  (ensure (null (find-edge-between-vertexes graph-directed 'a 'b
                                            :error-if-not-found? nil))))

;;; ---------------------------------------------------------------------------

(addtest (test-basic-graph-properties)
  (delete-edge-between-vertexes graph-directed 'a 'b)
  (ensure-same (size (graph-edges graph-directed)) 3))

#|

(deftestsuite cl-graph-test-traversal (cl-graph-test)
  ((g (make-container 'graph-container)))
  :setup (loop for (src dst) in '((a b) (a c) (a d) (b e) 
                                  (b f) (d g) (d h) (h i)
                                  (h j)) do
               (add-edge-between-vertexes g src dst :edge-type :directed)))

#|

a - b - e
      - f
  - c 
  - d - g
      - h - i
          - j

|#

(addtest (cl-graph-test-traversal)
  (let ((result nil))
    (traverse-elements
     g :depth (lambda (v) (push (element v) result)))
    (ensure-same (reverse result) 
                 '(e f b c g i j h d a) :test #'equal)))

(addtest (cl-graph-test-traversal)
  (let ((result nil))
    (traverse-elements
     g :breadth (lambda (v) (push (element v) result)))
    ;(print (reverse result))
    (ensure-same (reverse result) 
                 '(a b c d e f g h i j) :test #'equal)))
    
|#
    
;;; ---------------------------------------------------------------------------
;;; test-replace-vertex
;;; ---------------------------------------------------------------------------

(deftestsuite test-replace-vertex (test-basic-graph-properties) ())

;;; ---------------------------------------------------------------------------

(addtest (test-replace-vertex)
  test-directed
  (let ((b (find-vertex graph-directed 'b))
        (x (make-vertex-for-graph graph-directed :element 'x)))
    (replace-vertex graph-directed b x)
    (ensure (find-vertex graph-directed 'x))
    (ensure (not (find-vertex graph-directed 'b nil)))
    (ensure-same (edge-count (find-vertex graph-directed 'x)) 2 :test =)
    (ensure (find-edge-between-vertexes graph-directed 'a 'x))
    (ensure (find-edge-between-vertexes graph-directed 'x 'd))))

;;; ---------------------------------------------------------------------------

(addtest (test-replace-vertex)
  test-undirected
  (let ((b (find-vertex graph-undirected 'b))
        (x (make-vertex-for-graph graph-undirected :element 'x)))
    (replace-vertex graph-undirected b x)
    (ensure (find-vertex graph-undirected 'x))
    (ensure (not (find-vertex graph-undirected 'b nil)))
    (ensure-same (edge-count (find-vertex graph-undirected 'x)) 2 :test =)
    (ensure (find-edge-between-vertexes graph-undirected 'a 'x))
    (ensure (find-edge-between-vertexes graph-undirected 'x 'd))))

;;; ---------------------------------------------------------------------------
;;; change vertex value
;;; ---------------------------------------------------------------------------

(deftestsuite test-change-vertex-value (test-basic-graph-properties) ())

(addtest (test-change-vertex-value)
  test-undirected
  (let ((b (find-vertex graph-undirected 'b)))
    (setf (element b) 'x)
    (ensure (find-vertex graph-undirected 'x))
    (ensure (not (find-vertex graph-undirected 'b nil)))
    (ensure-same (edge-count (find-vertex graph-undirected 'x)) 2 :test =)
    (ensure (find-edge-between-vertexes graph-undirected 'a 'x))
    (ensure (find-edge-between-vertexes graph-undirected 'x 'd))))

;;;

(deftestsuite test-rootp (cl-graph-test)
  ((g (make-container 'graph-container
		      :default-edge-type :directed)))
  (:setup
    (loop for (v1 . v2) in '((a . b) (a . c) (b . d) (c . e)) do
	 (add-edge-between-vertexes g v1 v2))))

(addtest (test-rootp)
  directed-edges
  (ensure (directed-edge-p (first-item (graph-edges g)))))

(addtest(test-rootp)
  test-source-vertex
  (ensure (rootp (find-vertex g 'a))))

(addtest(test-rootp)
  test-sink-vertex
  (ensure-null (rootp (find-vertex g 'e))))

(addtest(test-rootp)
  test-middle-vertex
  (ensure-null (rootp (find-vertex g 'b))))


  
    
