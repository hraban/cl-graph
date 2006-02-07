(in-package metabang.graph)

(defun roots-and-child-function->graph (roots child-function max-depth)
  (let ((g (make-graph 'graph-container)))
    (labels ((init-vertex (vertex depth)
               (when (or (not max-depth) (< depth max-depth))
                 (unless (find-vertex g vertex nil)
                   (add-vertex g vertex)
                   (loop for child in (funcall child-function vertex) do
                         (init-vertex child (1+ depth))
                         (add-edge-between-vertexes g vertex child))))))
      (loop for root in roots do
            (init-vertex root 0)))
    g))

;;; ---------------------------------------------------------------------------

(defun class-hierarchy->dot (base-class-or-classes output &key (filter (constantly t)))  
  (metabang.graph:graph->dot 
   (roots-and-child-function->graph
    (ensure-list base-class-or-classes)
    (lambda (cname)
      (when (funcall filter cname)
        (mapcar #'class-name (mopu:direct-subclasses (find-class cname)))))
    nil) 
   output
   :graph-formatter (lambda (g stream)
                      (declare (ignore g))
                      (format stream "rankdir=LR"))
   
   :vertex-labeler (lambda (vertex stream)
                     (format stream "~(~A~)" (symbol-name (element vertex))))

   :vertex-formatter (lambda (vertex stream)
                       (when (subtypep (element vertex) 'containers::concrete-container)
                         (format stream "color=\"blue\", style=\"filled\", fontcolor=\"white\", fillcolor=\"blue\"")))))

;;; ---------------------------------------------------------------------------

#+Test
(class-hierarchy->dot 'abstract-container
                      nil
                      :filter (lambda (class-name)
                                (not (subtypep class-name 'containers::abstract-generator))))


#+Test
(class-hierarchy->dot '(containers::abstract-generator
                        containers::transforming-iterator-mixin
                        containers::basic-filtered-iterator-mixin
                        containers::circular-iterator-mixin)
                      "thousand-parsers:iterators.dot")