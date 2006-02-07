(in-package metabang.graph)

;;; ---------------------------------------------------------------------------

(lift:deftestsuite test-vertex-triangle-count ()
  ((g (make-container 'graph-container))))

(lift:deftestsuite test-vertex-triangle-count-1 (test-vertex-triangle-count)
  ()
  (:setup
   (loop for v in '(a b c d e f g h) do (add-vertex g v))
   (loop for (s d) in '((a b) (b c) (a c) (b d) (d e) (d f) (d g) (e f) (f g) (g h)) do
         (add-edge-between-vertexes g s d))))

(lift:addtest (test-vertex-triangle-count-1)
  (lift:ensure-same (vertex-triangle-count (find-vertex g 'a)) 1 :test '=))


(lift:addtest (test-vertex-triangle-count-1)
  (lift:ensure-same (vertex-triangle-count (find-vertex g 'd)) 2 :test '=))

(lift:addtest (test-vertex-triangle-count-1)
  (lift:ensure-same (vertex-triangle-count (find-vertex g 'h)) 0 :test '=)) 

(lift:deftestsuite test-vertex-triangle-count-2 (test-vertex-triangle-count)
  ()
  (:setup
   (loop for v in '(a b c d e) do (add-vertex g v))
   (loop for (s d) in '((a b) (b c) (a c) (c d) (c e)) do
         (add-edge-between-vertexes g s d))))

(lift:addtest (test-vertex-triangle-count-2)
  (lift:ensure-same (vertex-triangle-count (find-vertex g 'c)) 1 :test '=)
  (lift:ensure-same (vertex-triangle-count (find-vertex g 'd)) 0 :test '=))

(lift:addtest (test-vertex-triangle-count-2)
  (lift:ensure-same (average-local-clustering-coefficient g) 
               (float (/ 13 30)) :test 'samep))