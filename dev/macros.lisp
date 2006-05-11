;;;-*- Mode: Lisp; Package: metabang.graph -*-

(in-package #:metabang.graph)

;;?? Gary King 2006-01-30: 
;;?? Face it, I have no idea why we need this anymore... but i'm sure we do
(defmacro with-changing-vertex ((vertex) &body body)
  "This is used to maintain consistency when changing the value of vertex elements while iterating over the vertexes..." 
  (with-variables (v g)
    `(let* ((,v ,vertex)
            (,g (graph ,v)))
       (delete-item-at (graph-vertexes ,g) 
                       (funcall (vertex-key ,g) (element ,v)))  
       ,@body
       (setf (item-at (graph-vertexes ,g) 
                      (funcall (vertex-key ,g) (element ,v))) ,v))))
