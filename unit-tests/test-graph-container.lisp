(in-package cl-graph-test)

;;; ---------------------------------------------------------------------------
;;; utilities
;;; ---------------------------------------------------------------------------

(defun make-simple-test-graph ()
  (let ((g (make-container 'graph-container))) 
    (loop for v in '(a b c d e) do
          (add-vertex g v))
    (loop for (v1 . v2) in '((a . b) (a . c) (b . d) (c . e)) do
          (add-edge-between-vertexes g v1 v2))
    g))

;;; ---------------------------------------------------------------------------
;;; tests
;;; --------------------------------------------------------------------------- 

(deftestsuite test-graph-container () ())

;;; ---------------------------------------------------------------------------

(addtest (test-graph-container)
  test-simple-copying
  (let ((g1 (make-simple-test-graph))
        (g2 nil))
    (setf g2 (copy-top-level g1))
    (ensure-same (size g1) (size g2))
    (iterate-vertexes
     g1 (lambda (v)
          (ensure (find-vertex g2 (value v)))))
    (iterate-edges 
     g1 (lambda (e)
          (ensure (find-edge-between-vertexes 
                   g2 (value (source-vertex e))
                   (value (target-vertex e))))))))

;;; ---------------------------------------------------------------------------

;; fails because find-edge-between-vertexes for graph containers doesn't
;; care about the graph...
(addtest (test-graph-container)
  test-find-edge-between-vertexes
  (let ((g1 (make-simple-test-graph))
        (g2 nil))
    (setf g2 (copy-top-level g1))
    
    (ensure (not 
             (find-edge-between-vertexes g2 (find-vertex g1 'a) (find-vertex g1 'b))))))

;;; ---------------------------------------------------------------------------

(addtest (test-graph-container)
  test-empty!
  (let ((g1 (make-simple-test-graph)))
    (empty! g1)
    (ensure-same (size g1) 0)))

;;; ---------------------------------------------------------------------------
;;; vertex test 
;;; ---------------------------------------------------------------------------

;;?? should be in test-graph and work for every graph container type

(addtest (test-graph-container)
  no-vertex-test
  (let ((g (make-container 'graph-container)))
    (loop for (src dst) in '((a b) (a c) (c d) (a d) (d e) (e f) (b f)) do
          (add-edge-between-vertexes g (list src) (list dst)))
    (ensure-same (size g) 14 :test '=)))

(addtest (test-graph-container)
  vertex-test
  (let ((g (make-container 'graph-container :vertex-test #'equal)))
    (loop for (src dst) in '((a b) (a c) (c d) (a d) (d e) (e f) (b f)) do
          (add-edge-between-vertexes g (list src) (list dst)))
    (ensure-same (size g) 6 :test '=)))
