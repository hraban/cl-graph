(in-package metabang.graph)

(deftestsuite test-connected-component ()
  ())

;;; ---------------------------------------------------------------------------

(defun make-connected-component-graph-1 ()
  (let ((g (make-container 'graph-container)))
    (loop for label in '(wk-6-0 wp-5-1 wp-1-2 wp-2-3 wb-1-1
                         wp-4-4 bp-5-6 bk-6-5 bb-5-7 bp-2-4
                         bp-2-6 bp-1-5) do
          (add-vertex g label))
    (loop for (source target) in '((wk-6-0 wp-5-1)
                                   (wp-1-2 wp-2-3)
                                   (wb-1-1 wp-4-4)
                                   (bp-5-6 bk-6-5)
                                   (bk-6-5 bb-5-7)
                                   (bb-5-7 bp-2-4)
                                   (bp-2-6 bp-1-5)
                                   (bp-1-5 bp-2-4)) do
          (add-edge-between-vertexes g source target :edge-type :directed
                                     :value :defend))
    (loop for (source target) in '((bk-6-5 wp-4-4)) do
          (add-edge-between-vertexes g source target :edge-type :directed 
                                     :value :attack))
    (loop for (source target) in '((wp-2-3 bp-2-4)) do
          (add-edge-between-vertexes g source target :edge-type :undirected))
    g))

;;; ---------------------------------------------------------------------------

(addtest (test-connected-component)
  test-1
  (let ((g (make-connected-component-graph-1)))
    (ensure-same
     (mapcar #'size (find-connected-components g)) '(2 10) :test 'set-equal)))

;;; ---------------------------------------------------------------------------

(addtest (test-connected-component)
  test-connected-component-count-1
  (let ((g (make-connected-component-graph-1)))
    (ensure-same (connected-component-count g) 2 :test '=)))
