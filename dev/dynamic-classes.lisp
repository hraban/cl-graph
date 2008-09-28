(in-package cl-graph)

(defmethod make-graph ((classes list) &rest args)
  (let ((name (dynamic-classes:find-or-create-class 'basic-graph classes))) 
    (apply #'make-instance name args)))

