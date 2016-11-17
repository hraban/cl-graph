(in-package #:metabang.graph)

(defun roots-and-child-function->graph (roots child-function max-depth)
  (let ((g (make-graph 'dot-graph :vertex-test #'equal)))
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

;; Use POIU to build a reified ASDF dependency graph.
(asdf:load-system "poiu") ;; Use POIU for its precise action graph

(in-package :poiu)
(uiop-debug)

(defun print-action-label (action s)
  (destructuring-bind (op . c) action
    (let ((o (if (symbolp op) op (type-of op))))
      (format s "~(~a~) ~{~a~^ ~}" o (component-find-path c)))))

(defun asdf-dependency-graph (system &optional (max-depth 100))
  (let* ((system (asdf:find-system system))
         (op (asdf:make-operation 'asdf:load-op))
         (plan (make-plan 'parallel-plan op system :force :all)))
  (cl-graph:graph->dot
   (cl-graph::roots-and-child-function->graph
    (list (cons op system))
    (lambda (action)
      (if-let (children (action-map (plan-children plan) action)) (action-map-keys children)))
    max-depth)
   #p"/tmp/out.dot"
   :vertex-labeler (lambda (v s) (print-action-label (cl-graph:element v) s))
   :edge-labeler (lambda (e s) (declare (ignore e s)) ()))))

(asdf-dependency-graph "fare-csv")
(uiop:run-program "dot -Tpdf -o /tmp/out.pdf /tmp/out.dot && xpdf /tmp/out.pdf")
