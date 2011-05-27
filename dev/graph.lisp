;;;-*- Mode: Lisp; Package: metabang.graph -*-

#|
$Id: graph.lisp,v 1.30 2005/09/07 16:17:06 gwking Exp $

Author: Gary W. King, et. al.

|#

#| NOTES

something is putting something on the vertexes plist's

|#


(in-package #:metabang.graph)

;;; classes

(defcondition graph-error (error)
  ((graph nil ir))
  (:export-p t)
  (:export-slots-p t)
  (:documentation "This is the root condition for errors that occur while running code in CL-Graph."))


(defcondition edge-error (graph-error)
  ((edge nil ir "The `edge` that is implicated in the condition."))
  (:export-p t)
  (:export-slots-p t)
  (:documentation "This is the root condition for graph errors that have to do with edges."))


(defcondition graph-vertex-not-found-error (graph-error)
  ((vertex nil ir "The vertex or value that could not be found in the graph."))
  (:report (lambda (c s)
             (format s "Vertex ~S not found in ~A" (vertex c) (graph c))))
  (:export-p t)
  (:export-slots-p t)
  (:documentation "This condition is signaled when a vertex can not be found in a graph."))


(defcondition graph-vertex-not-found-in-edge-error (edge-error)
  ((vertex nil ir))
  (:report (lambda (c s)
             (format s "Vertex ~S not found in ~A" (vertex c) (edge c))))
  (:export-p t)
  (:documentation "This condition is signaled when a vertex can not be found in an edge."))


(defcondition graph-edge-not-found-error (graph-error)
  ((vertex-1 nil ir "One of the vertexes for which no connecting edge could be found.")
   (vertex-2 nil ir "One of the vertexes for which no connecting edge could be found."))
  (:report (lambda (c s)
             (format s "Edge between ~S and ~S not found in ~A"
                     (vertex-1 c) (vertex-2 c) (graph c))))
  (:export-p t)
  (:export-slots-p t)
  (:documentation "This condition is signaled when an edge cannot be found in a graph."))


(defclass* basic-vertex (container-node-mixin)
  ((depth-level 0 ia :type number "`Depth-level` is used by some algorithms for bookkeeping.  [?? Should be in a mixin]")
   (vertex-id 0 ir "`Vertex-id` is used internally to keep track of vertexes.")
   (element :unbound ia :accessor value "The `element` is the value that this vertex represents.")
   (tag nil ia "The `tag` slot is used by some algorithms to keep track of which vertexes have been visited.")
   (graph nil ia "The graph in which this vertex is contained.")
   (color nil ia "The `color` slot is used by some algorithms for bookkeeping.")
   (rank nil ia "The `rank` is used by some algorithms for bookkeeping.  [?? Should be in a mixin]")
   (previous-node nil ia "`Previous-node` is used by some algorithms for bookkeeping. [?? Should be in a mixin]")
   (next-node nil ia "`Next-node` is used by some algorithms for bookkeeping.  [?? Should be in a mixin]")
   (discovery-time -1 ia "`Discovery-time` is used by some algorithms for bookkeeping.  [?? Should be in a mixin]")
   (finish-time -1 ia "`Finish-time` is used by some algorithms for bookkeeping.  [?? Should be in a mixin]"))
  (:export-p t)
  (:export-slots vertex-id tag rank color previous-node next-node
                 discovery-time finish-time)
  (:make-load-form-p t)
  (:documentation "This is the root class for all vertexes in CL-Graph."))


(defmethod initialize-instance :after ((object basic-vertex) &key graph vertex-id)
  (when (and graph (not vertex-id))
    (setf (slot-value object 'vertex-id)
          (largest-vertex-id graph))
    (incf (slot-value graph 'largest-vertex-id))))


(defmethod print-object ((vertex basic-vertex) stream)
  (print-unreadable-object (vertex stream :identity nil)
    (format stream "~A"
            (if (and (slot-exists-p vertex 'element) (slot-boundp vertex 'element))
              (element vertex) "#unbound#"))))


(defclass* basic-edge ()
  ((edge-id 0 ia "The `edge-id` is used internally by CL-Graph for bookkeeping.")
   (element nil ia :accessor value :initarg :value)
   (tag nil ia "The `tag` is used by some algorithms for bookkeeping. [?? Should probably be in a mixin]")
   (graph nil ir "The `graph` of which this edge is a part.")
   (color nil ia "The `color` is used by some algorithms for bookkeeping. [?? Should probably be in a mixin]"))
  (:export-p t)
  (:export-slots edge-id element tag color)
  (:make-load-form-p t)
  (:documentation "This is the root class for all edges in CL-Graph."))


(defmethod initialize-instance :after ((object basic-edge) &key graph edge-id)
  (when (and graph (not edge-id))
    (setf (slot-value object 'edge-id)
          (largest-edge-id graph))
    (incf (slot-value graph 'largest-edge-id))))


(defmethod print-object ((object basic-edge) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "<~A ~A>" (vertex-1 object) (vertex-2 object))))


(defclass* directed-edge-mixin () ()
  (:export-p t)
  (:documentation "This mixin class is used to indicate that an edge is directed."))


(defclass* weighted-edge-mixin ()
  ((weight 1d0 ia "The value of the weight of this edge. Defaults to 1.0d0"))
  :export-slots
  (:export-p t)
  (:documentation "This mixin class adds a `weight` slot to an edge."))


(defmethod weight ((edge basic-edge)) (values 1.0))


(defclass* basic-graph ()
  ((graph-vertexes :unbound ir)
   (graph-edges :unbound ir)
   (largest-vertex-id 0 r)
   (largest-edge-id 0 r)
   (vertex-class 'basic-vertex ir
                 "The class of the vertexes in the graph. This must extend the base-class for vertexes of the graph type. E.g., all vertexes of a graph-container must extend graph-container-vertex.")
   (directed-edge-class 'basic-directed-edge ir
                        "The class used to create directed edges in the graph. This must extend the base-class for edges of the graph type and directed-edge-mixin. E.g., the directed-edge-class of a graph-container must extend graph-container-edge and directed-edge-mixin.")
   (undirected-edge-class 'basic-edge ir
                          "The class used to create undirected edges in the graph. This must extend the base-class for edges of the graph type. E.g., all edges of a graph-container must extend graph-container-edge")
   (contains-directed-edge-p nil ar
                             "Returns true if graph contains at least one directed edge. [?? Not sure if this is really keep up-to-date.]")
   (contains-undirected-edge-p nil ar
                               "Returns true if graph contains at least one undirected edge. [?? Not sure if this is really keep up-to-date.]")
   (vertex-test #'eq ir)
   (vertex-key #'identity ir)
   (edge-test #'eq ir)
   (edge-key #'identity ir)
   (default-edge-type nil ir
     "The default edge type for the graph. This should be one of :undirected or :directed.")
   (default-edge-class nil ir
     "The default edge class for the graph."))
  (:make-load-form-p t)
  (:export-slots vertex-class directed-edge-class undirected-edge-class
                 default-edge-type default-edge-class)
  (:default-initargs
    :initial-size 25)
  (:documentation "This is the root class for all graphs in CL-Graph."))


(defmethod initialize-instance :after ((object basic-graph) &key initial-size
                                       &allow-other-keys)
  (setf (slot-value object 'graph-vertexes)
        (make-vertex-container object initial-size))
  (setf (slot-value object 'graph-edges)
        (make-edge-container object initial-size)))


(defmethod print-object ((graph basic-graph) stream)
  (print-unreadable-object (graph stream :type t :identity t)
    (format stream "[~A,~A]" (size graph) (edge-count graph))))


;;; internals

(defmethod add-vertex
    ((graph basic-graph) (value basic-vertex) &key if-duplicate-do)
  (declare (ignore if-duplicate-do))
  (values value))


(defmethod make-vertex-for-graph ((graph basic-graph) &rest args &key
                                  (vertex-class (vertex-class graph))
                                  &allow-other-keys)
  (remf args :vertex-class)
  (assert (subtypep vertex-class (vertex-class graph)) nil
          "Vertex class '~A' must be a subtype of ~A" vertex-class (vertex-class graph))
  (apply #'make-instance vertex-class :graph graph args))


(defmethod make-edge-for-graph ((graph basic-graph)
                                (vertex-1 basic-vertex) (vertex-2 basic-vertex)
                                &rest args &key
                                (edge-type (default-edge-type graph))
                                (edge-class (default-edge-class graph))
                                &allow-other-keys)
  (remf args :edge-class)
  (remf args :edge-type)
  (assert (or (null edge-type)
              (eq edge-type :directed)
              (eq edge-type :undirected)) nil
          "Edge-type must be nil, :directed or :undirected.")

  (assert (or (null edge-class)
              (subtypep edge-class (directed-edge-class graph))
              (subtypep edge-class (undirected-edge-class graph))) nil
          "Edge-class must be nil or a subtype of ~A or ~A"
          (undirected-edge-class graph)
          (directed-edge-class graph))

  (apply #'make-instance
         (or edge-class
             (ecase edge-type
               (:directed (directed-edge-class graph))
               (:undirected (undirected-edge-class graph))
               ((nil) nil))
             (undirected-edge-class graph))
         :graph graph
         :vertex-1 vertex-1 :vertex-2 vertex-2 args))


(defmethod make-graph ((graph-type symbol) &rest args &key &allow-other-keys)
  (apply #'make-instance graph-type args))

;;; generic implementation

(defmethod undirected-edge-p ((edge basic-edge))
  (not (directed-edge-p edge)))


(defmethod directed-edge-p ((edge basic-edge))
  (typep edge 'directed-edge-mixin))


(defmethod tagged-edge-p ((edge basic-edge))
  (tag edge))


(defmethod untagged-edge-p ((edge basic-edge))
  (null (tag edge)))


(defmethod tag-all-edges ((graph basic-graph))
  (iterate-edges
   graph
   (lambda (e)
     (setf (tag e) t))))


(defmethod tag-all-edges ((vertex basic-vertex))
  (iterate-edges
   vertex
   (lambda (e)
     (setf (tag e) t))))


(defmethod untag-all-edges ((graph basic-graph))
  (iterate-edges
   graph
   (lambda (e)
     (setf (tag e) nil))))


(defmethod untag-all-edges ((vertex basic-vertex))
  (iterate-edges
   vertex
   (lambda (e)
     (setf (tag e) nil))))


(defmethod untag-edges ((edges list))
  (iterate-nodes
   edges
   (lambda (e)
     (setf (tag e) nil))))


(defmethod tag-edges ((edges list))
  (iterate-nodes
   edges
   (lambda (e)
     (setf (tag e) t))))



(defmethod (setf element) :around ((value t) (vertex basic-vertex))
  (with-changing-vertex (vertex)
    (call-next-method)))


;; :ignore, :force, :replace, <function>

(defmethod add-vertex ((graph basic-graph) (value t) &rest args &key
                       (if-duplicate-do :ignore) &allow-other-keys)
  (remf args :if-duplicate-do)
  (let ((existing-vertex (find-vertex graph value nil)))
    (labels ((make-it ()
               (apply #'make-vertex-for-graph graph :element value args))
             (add-it (why)
               (values (add-vertex graph (make-it)) why)))
      (if existing-vertex
        (cond ((eq if-duplicate-do :ignore)
               (values existing-vertex :ignore))

              ((eq if-duplicate-do :force)
               (add-it :force))

              ((eq if-duplicate-do :replace)
               (replace-vertex graph existing-vertex (make-it)))

              ((eq if-duplicate-do :replace-value)
               (setf (element existing-vertex) value)
               (values existing-vertex :replace-value))

              ((eq if-duplicate-do :error)
               (error "Attempting to insert a duplicate node in graph ~a" graph))

              (t
               (values (funcall if-duplicate-do existing-vertex)
                       :duplicate)))

        ;; not found, add
        (add-it :new)))))


(defmethod replace-vertex ((graph basic-graph) (old basic-vertex) (new basic-vertex))
  ;; we need the graph and the new vertex to reference each other
  ;; we need every edge of the old vertex to use the new-vertex
  ;; we need to remove the old vertex
  ;;
  ;; since I'm tired today, let's ignore trying to make this elegant

  ;; first, we connect the edges to the new vertex so that they don't get deleted
  ;; when we delete the old vertex
  (iterate-edges
   old
   (lambda (e)
     (if (eq (vertex-1 e) old)
       (setf (slot-value e 'vertex-1) new) (setf (slot-value e 'vertex-2) new))
     (add-edge-to-vertex e new)))

  (delete-vertex graph old)
  (add-vertex graph new))


(defmethod add-edge-between-vertexes ((graph basic-graph) (value-1 t) (value-2 t)
                                      &rest args &key (if-duplicate-do :ignore)
                                      &allow-other-keys)
  (declare (ignore if-duplicate-do)
           (dynamic-extent args))
  (let ((v1 (or (find-vertex graph value-1 nil)
                (add-vertex graph value-1 :if-duplicate-do :ignore)))
        (v2 (or (find-vertex graph value-2 nil)
                (add-vertex graph value-2  :if-duplicate-do :replace))))
    (apply #'add-edge-between-vertexes graph v1 v2 args)))

;;; to-do - add-edge-between-vertexes needs to be able to grab the weight and
;;; color from edges that inherit from weight and color mixins

(defmethod add-edge-between-vertexes ((graph basic-graph)
                                      (v-1 basic-vertex) (v-2 basic-vertex)
                                      &rest args &key
                                      (value nil) (if-duplicate-do :ignore)
                                      &allow-other-keys)
  (declare (dynamic-extent args))
  (remf args :if-duplicate-do)

  (let ((edge (find-edge-between-vertexes graph v-1 v-2 :error-if-not-found? nil)))
    (flet ((add-it (why)
             (values (add-edge
                      graph
                      (apply #'make-edge-for-graph graph v-1 v-2 args))
                     why)))
      (if edge
        (cond
         ((eq if-duplicate-do :ignore)
          (values edge :ignore))

         ((eq if-duplicate-do :force)
          (add-it :force))

         ((eq if-duplicate-do :force-if-different-value)
          (if (equal (value edge) value)
            (values :ignore)
            (add-it :force)))


         ((eq if-duplicate-do :replace)
          (warn "replace edges isn't really implemented, maybe you can use :replace-value")
          (delete-edge graph edge)
          (add-it :replace))

         ((eq if-duplicate-do :replace-value)
          (setf (element edge) value)
          (values edge :replace-value))

         (t
          (setf edge (funcall if-duplicate-do edge))
          (values edge :duplicate)))

        ;; not found, add
        (add-it :new)))))



(defmethod add-edge-to-vertex ((edge basic-edge) (vertex basic-vertex))
  (values))


(defmethod find-edge-between-vertexes
    ((graph basic-graph) (value-1 t) (value-2 t)
     &key (error-if-not-found? t))
  (let* ((v1 (find-vertex graph value-1 error-if-not-found?))
	 (v2 (find-vertex graph value-2 error-if-not-found?)))
    (or (and v1 v2 (find-edge-between-vertexes graph v1 v2))
	(when error-if-not-found?
	  (error 'graph-edge-not-found-error
		 :graph graph :vertex-1 v1 :vertex-2 v2)))))


(defmethod delete-edge-between-vertexes ((graph basic-graph)
                                         (value-or-vertex-1 t)
                                         (value-or-vertex-2 t) &rest args)
  (let ((edge (apply #'find-edge-between-vertexes
                     graph value-or-vertex-1 value-or-vertex-2 args)))
    (when edge
      (delete-edge graph edge))))


(defmethod delete-edge :after ((graph basic-graph) (edge basic-edge))
  (delete-item (graph-edges graph) edge)
  edge)


(defmethod delete-all-edges :after ((graph basic-graph))
  (empty! (graph-edges graph))
  graph)


(defmethod delete-vertex ((graph basic-graph) value-or-vertex)
  (delete-vertex graph (find-vertex graph value-or-vertex)))


(defmethod delete-vertex ((graph basic-graph) (vertex basic-vertex))
  (unless (eq graph (graph vertex))
    (error 'graph-vertex-not-found-error
           :graph graph :vertex vertex))

  (iterate-edges
   vertex
   (lambda (edge)
     (delete-edge graph edge)))

  (empty! (vertex-edges vertex))
  (values vertex graph))


(defmethod delete-vertex :after ((graph basic-graph)
                                 (vertex basic-vertex))
  (setf (slot-value vertex 'graph) nil)
  (delete-item-at (graph-vertexes graph)
                  (funcall (vertex-key graph) (element vertex))))


(defmethod insert-item ((graph basic-graph) value)
  (add-vertex graph value))


(defmethod source-edges ((vertex basic-vertex) &optional filter)
  (collect-using #'iterate-source-edges filter vertex))


(defmethod target-edges ((vertex basic-vertex) &optional filter)
  (collect-using #'iterate-target-edges filter vertex))


(defmethod child-vertexes (vertex &optional filter)
  (collect-using #'iterate-children filter vertex))


(defmethod parent-vertexes (vertex &optional filter)
  (collect-using #'iterate-parents filter vertex))


(defmethod neighbor-vertexes (vertex &optional filter)
  (collect-using #'iterate-neighbors filter vertex))


(defmethod adjacentp ((graph basic-graph) vertex-1 vertex-2)
  (adjacentp graph (find-vertex graph vertex-1) (find-vertex graph vertex-2)))


(defmethod adjacentp ((graph basic-graph) (vertex-1 basic-vertex) (vertex-2 basic-vertex))
  (iterate-neighbors
   vertex-1
   (lambda (vertex)
     (when (eq vertex vertex-2)
       (return-from adjacentp t))))
  (values nil))


(defmethod number-of-neighbors (vertex)
  (count-using #'iterate-neighbors nil vertex))


(defmethod in-cycle-p ((graph basic-graph) (vertex t))
  (in-cycle-p graph (find-vertex graph vertex)))


(defmethod renumber-vertexes ((graph basic-graph))
  (let ((count 0))
    (iterate-vertexes graph (lambda (vertex)
                              (setf (slot-value vertex 'vertex-id) count)
                              (incf count)))
    (setf (slot-value graph 'largest-vertex-id) count)))


(defmethod renumber-edges ((graph basic-graph))
  (let ((count 0))
    (iterate-edges graph (lambda (vertex)
                           (setf (slot-value vertex 'edge-id) count)
                           (incf count)))
    (setf (slot-value graph 'largest-edge-id) count)))


(deprecated
  (defmethod container->list ((graph basic-graph))
    (collect-elements (graph-vertexes graph))))


(defmethod add-vertex :before ((graph basic-graph) (vertex basic-vertex)
                               &key &allow-other-keys)

  (assert (typep vertex (vertex-class graph)))
  (setf (item-at (graph-vertexes graph)
                 (funcall (vertex-key graph) (element vertex))) vertex
        (slot-value vertex 'graph) graph))


(defmethod add-edge :before ((graph basic-graph) (edge basic-edge) &key force-new?)
  (declare (ignore force-new?))
  (insert-item (graph-edges graph) edge)
  (setf (slot-value edge 'graph) graph)
  (if (subtypep (class-name (class-of edge)) 'directed-edge-mixin)
    (progn (setf (contains-directed-edge-p graph) t))
    (progn (setf (contains-undirected-edge-p graph) t))))


(defmethod find-vertex ((graph basic-graph) (value t)
                        &optional (error-if-not-found? t))
  (or (find-item (graph-vertexes graph) (funcall (vertex-key graph) value))
      (when error-if-not-found?
	(error 'graph-vertex-not-found-error :vertex value :graph graph))))

(defmethod find-vertex ((graph basic-graph) (vertex basic-vertex)
                        &optional (error-if-not-found? t))
  (cond ((eq graph (graph vertex))
	 vertex)
	(t
	 (when error-if-not-found?
	   (error 'graph-vertex-not-found-error 
		  :vertex vertex :graph graph)))))

(defmethod find-vertex ((edge basic-edge) (value t)
                        &optional (error-if-not-found? t))
  (iterate-vertexes
   edge
   (lambda (vertex)
     (when (funcall (vertex-test (graph edge))
                    (funcall (vertex-key (graph edge)) (element vertex)) value)
       (return-from find-vertex vertex))))
  (when error-if-not-found?
    (error 'graph-vertex-not-found-in-edge-error :vertex value :edge edge)))


(defmethod search-for-vertex ((graph basic-graph) (vertex basic-vertex)
                              &key (key (vertex-key graph)) (test 'equal)
                              (error-if-not-found? t))
  (or (search-for-node (graph-vertexes graph) vertex :test test :key key)
      (when error-if-not-found?
	(error "~A not found in ~A" vertex graph))))

(defmethod search-for-vertex ((graph basic-graph) (vertex t)
                              &key (key (vertex-key graph)) (test 'equal)
                              (error-if-not-found? t))
  (or (search-for-element (graph-vertexes graph) vertex :test test :key key)
      (when error-if-not-found?
	(error "~A not found in ~A" vertex graph))))

(defmethod iterate-elements ((graph basic-graph) fn)
   (iterate-elements (graph-vertexes graph)
                     (lambda (vertex) (funcall fn (element vertex)))))


(defmethod iterate-nodes ((graph basic-graph) fn)
   (iterate-nodes (graph-vertexes graph) fn))


(defmethod iterate-vertexes ((graph basic-graph) fn)
   (iterate-nodes (graph-vertexes graph) fn))


(defmethod iterate-vertexes ((edge basic-edge) fn)
  (funcall fn (vertex-1 edge))
  (funcall fn (vertex-2 edge)))


(defmethod size ((graph basic-graph))
  (size (graph-vertexes graph)))


(defmethod edges ((graph basic-graph))
  (collect-using #'iterate-edges nil graph))


(defmethod edges ((vertex basic-vertex))
  (collect-using #'iterate-edges nil vertex))


(deprecated
  "Use size instead"
  (defmethod vertex-count ((graph basic-graph))
    (size graph)))


(defmethod vertexes ((graph basic-graph))
  (collect-elements (graph-vertexes graph)))


(defmethod source-edge-count ((vertex basic-vertex))
  (count-using 'iterate-source-edges nil vertex))


(defmethod target-edge-count ((vertex basic-vertex))
  (count-using 'iterate-target-edges nil vertex))


(defmethod graph-roots ((graph basic-graph))
  (collect-elements (graph-vertexes graph) :filter #'rootp))


(defmethod graph-leafs ((graph basic-graph))
  (collect-elements (graph-vertexes graph) :filter #'leafp))


(defmethod rootp ((vertex basic-vertex))
  ;;?? this is inefficient in the same way that (zerop (length <list>)) is...
  (zerop (target-edge-count vertex)))


(defmethod leafp ((vertex basic-vertex))
  (zerop (source-edge-count vertex)))


(defmethod find-vertex-if ((graph basic-graph) fn &key key)
  (iterate-vertexes graph
                    (lambda (v)
                      (when (funcall fn (if key (funcall key v) v))
                        (return-from find-vertex-if v))))
  (values nil))


(defmethod find-vertex-if ((edge basic-edge) fn &key key)
  (iterate-vertexes edge
                    (lambda (v)
                      (when (funcall fn (if key (funcall key v) v))
                        (return-from find-vertex-if v))))
  (values nil))


(defmethod find-edge-if ((graph basic-graph) fn &key key)
  (iterate-edges graph
                 (lambda (e)
                   (when (funcall fn (if key (funcall key e) e))
                     (return-from find-edge-if e))))
  (values nil))


(defmethod find-edges-if ((graph basic-graph) fn)
  (collect-using 'iterate-edges fn graph))


(defmethod find-vertexes-if ((graph basic-graph) fn)
  (collect-using 'iterate-vertexes fn graph))


(defmethod empty! ((graph basic-graph))
  (empty! (graph-edges graph))
  (empty! (graph-vertexes graph))
  (renumber-edges graph)
  (renumber-vertexes graph)
  (values))


(defun neighbors-to-children (new-graph root &optional visited-list)
  (pushnew root visited-list)
  (iterate-neighbors
   root
   (lambda (c)
     (when (not (member c visited-list))
       (add-edge-between-vertexes
        new-graph (value root) (value c) :edge-type :directed)
       (neighbors-to-children new-graph c visited-list)))))


(defmethod generate-directed-free-tree ((graph basic-graph) root)
  (generate-directed-free-tree graph (find-vertex graph root)))


(defmethod force-undirected ((graph basic-graph))
  (iterate-edges
   graph
   (lambda (edge)
     (change-class edge (undirected-edge-class graph)))))



;;; traversal

(defmethod traverse-elements ((thing basic-graph) (style symbol) fn)
  (let ((marker (gensym)))
    (iterate-vertexes
     thing
     (lambda (vertex)
       (setf (tag vertex) marker)))

    (iterate-elements
     (graph-roots thing)
     (lambda (vertex)
       (traverse-elements-helper vertex style marker fn)))))


(defmethod traverse-elements-helper ((thing basic-vertex) (style (eql :depth)) marker fn)
  (when (eq (tag thing) marker)
    (setf (tag thing) nil)
    (iterate-children
     thing
     (lambda (vertex)
       (traverse-elements-helper vertex style marker fn)))

    (funcall fn thing)))


(defmethod traverse-elements-helper ((thing basic-vertex) (style (eql :breadth)) marker fn)
  (when (eq (tag thing) marker)
    (setf (tag thing) nil)
    (funcall fn thing))

  (iterate-neighbors
   thing
   (lambda (vertex)
     (when (eq (tag vertex) marker)
       (funcall fn vertex))))

  (iterate-neighbors
   thing
   (lambda (vertex)
     (when (eq (tag vertex) marker)
       (setf (tag vertex) nil)
       (traverse-elements-helper vertex style marker fn)))))

;; also in metatilites
(defun graph-search-for-cl-graph (states goal-p successors combiner
				  &key (state= #'eql) old-states
				  (new-state-fn (error "argument required")))
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner.
  Don't try the same state twice."
  (cond ((null states) nil)
        ((funcall goal-p (first states)) (first states))
        (t (graph-search-for-cl-graph
             (funcall
               combiner
               (funcall new-state-fn states successors state= old-states)
               (rest states))
             goal-p successors combiner
             :state= state=
             :old-states (adjoin (first states) old-states
                                 :test state=)
             :new-state-fn new-state-fn))))

(defmethod in-cycle-p ((graph basic-graph) (start-vertex basic-vertex))
  (let ((first-time? t))
    (not (null
          (graph-search-for-cl-graph
           (list start-vertex)
           (lambda (v)
             (if first-time?
               (setf first-time? nil)
               (eq (find-vertex graph v) start-vertex)))
           (lambda (v)
             (child-vertexes v))
           #'append
           :new-state-fn
           (lambda (states successors state= old-states)
             ;; Generate successor states that have not been seen before but
             ;; don't remove the start state.
             (remove-if
              #'(lambda (state)
                  (and (not (eq start-vertex state))
                       (or (member state states :test state=)
                           (member state old-states :test state=))))
              (funcall successors (first states)))))))))


(defmethod in-undirected-cycle-p
           ((graph basic-graph) (current basic-vertex)
            &optional (marked (make-container 'simple-associative-container))
            (previous nil))
  (block do-it
    (setf (item-at-1 marked current) t)
    (iterate-children current
                      (lambda (child)
                        (cond
                         ((eq child previous) nil)
                         ((item-at-1 marked child) (return-from do-it t))
                         (t
                          (in-undirected-cycle-p graph child marked current)))))))


(defmethod any-undirected-cycle-p ((graph basic-graph))
  (let ((marked (make-container 'simple-associative-container)))
    (iterate-vertexes graph (lambda (v)
                              (unless (item-at-1 marked v)
                                (when (in-undirected-cycle-p graph v marked)
                                  (return-from any-undirected-cycle-p v)))))
    (values nil)))


(defun remove-list (original target)
  "Removes all elements in original from target."
  (remove-if (lambda (target-element)
               (member target-element original))
             target))


(defun get-nodelist-relatives (node-list)
  "Collects set of unique relatives of nodes in node-list."
  (let ((unique-relatives nil))
    (dolist (node node-list)
      (setf unique-relatives
            (append-unique (neighbor-vertexes node) unique-relatives)))
    unique-relatives))


(defun get-transitive-closure (vertex-list &optional (depth nil))
  "Given a list of vertices, returns a combined list of all of the nodes
in the transitive closure(s) of each of the vertices in the list
(without duplicates).  Optional DEPTH limits the depth (in _both_ the
child and parent directions) to which the closure is gathered; default
nil gathers the entire closure(s)."
  (labels ((collect-transitive-closure (remaining visited depth)
             (if (and remaining
                      (typecase depth
                        (null t)
                        (fixnum (>= (decf depth) 0))))

              (let* ((non-visited-relatives     ;; list of relatives not yet visited
                       (remove-list visited
                                    (get-nodelist-relatives remaining)))
                      (visited-nodes            ;; list of nodes visited so far
                       (append-unique non-visited-relatives visited)))
                 (collect-transitive-closure non-visited-relatives
                                             visited-nodes
                                             depth))
               (values visited))))
    (collect-transitive-closure vertex-list vertex-list depth)))


(defmethod edge-count ((graph basic-graph))
  (count-using #'iterate-edges nil graph))


(defmethod edge-count ((vertex basic-vertex))
  (size (vertex-edges vertex)))


(defmethod topological-sort ((graph basic-graph))
  (assign-level graph 0)
  (sort (collect-elements (graph-vertexes graph))  #'<
        :key (lambda (x) (depth-level x))))


(defmethod assign-level ((graph basic-graph) (level number))
  (loop for node in (graph-roots graph)
        do (assign-level node 0)))


(defmethod assign-level ((node basic-vertex) (level number))
  (if (or (not (depth-level node))
          (> level (depth-level node)))
    (setf (depth-level node) level))
  (iterate-children node (lambda (x) (assign-level x (1+ level)))))


(defmethod depth ((graph basic-graph))
  (assign-level graph 0)
  (let ((depth 0))
    (iterate-vertexes graph (lambda (vertex)
                              (when (> (depth-level vertex) depth)
				(setf depth (depth-level vertex)))))
    depth))

;;; mapping

(defun map-paths (graph start-vertex length fn &key (filter (constantly t)))
  "Apply fn to each path that starts at start-vertex and is of exactly length
length"
  ;; a sort of depth first search
  (labels ((follow-path (next-vertex current-path length)
             (when (zerop length)
               (funcall fn (reverse current-path)))
             ; (format t "~%~A ~A ~A" current-path next-vertex length)
             (when (plusp length)
               (iterate-neighbors
                next-vertex
                (lambda (v)
                  (when (funcall filter v)
                    ;; no repeats
                    (unless (find-item current-path v)
                      (let ((new-path  (copy-list current-path)))
                        (follow-path v (push v new-path) (1- length))))))))))
    (iterate-neighbors
     start-vertex
     (lambda (v)
       (when (funcall filter v)
         (follow-path v (list v start-vertex) (1- length))))))
  (values graph))


(defun map-shortest-paths
    (graph start-vertex depth fn &key (filter (constantly t)))
  "Apply fn to each shortest path starting at `start-vertex` of depth `depth`. The `filter` predicate is used to remove vertexes from consideration."
  (let ((visited (make-container 'simple-associative-container
                                  :test #'equal)))
    (labels ((visit (p)
               (setf (item-at-1 visited p) t))
             (visited-p (p)
               (item-at-1 visited p))
             )
      (loop for n from 1 to (1- depth) do
            (map-paths graph start-vertex n
                       (lambda (p)
                         (visit (first (last p))))
                       :filter filter))
      ;(break)
      (visit start-vertex)
      (map-paths graph start-vertex depth
                 (lambda (p)
                   (unless (visited-p (first (last p)))
                     (funcall fn p)))
                 :filter filter))))


;;; utilities

(defun append-unique (list1 list2)
  (remove-duplicates (append list1 list2)))

;;; project-bipartite-graph

(defmethod project-bipartite-graph
           ((new-graph symbol) graph vertex-class vertex-classifier)
  (project-bipartite-graph
   (make-instance new-graph) graph vertex-class  vertex-classifier))


(defmethod project-bipartite-graph
           ((new-graph basic-graph) graph vertex-class vertex-classifier)
  (iterate-vertexes
   graph
   (lambda (v)
     (when (eq (funcall vertex-classifier v) vertex-class)
       (add-vertex new-graph (element v)))))

  (iterate-vertexes
   graph
   (lambda (v)
     (when (eq (funcall vertex-classifier v) vertex-class)
       (iterate-neighbors
        v
        (lambda (other-class-vertex)
          (iterate-neighbors
           other-class-vertex
           (lambda (this-class-vertex)
             (when (< (vertex-id v) (vertex-id this-class-vertex))
               (add-edge-between-vertexes
                new-graph (element v) (element this-class-vertex)
                :if-duplicate-do (lambda (e) (incf (weight e))))))))))))

  new-graph)

#+Test
(pro:with-profiling
  (setf (ds :g-5000-m-projection)
        (project-bipartite-graph
         'undirected-graph-container
         (ds :g-5000)
         :m
         (lambda (v)
           (let ((vertex-class (aref (symbol-name (element v)) 0)))
             (cond ((member vertex-class '(#\a #\b) :test #'char-equal)
                    :m)
                   ((member vertex-class '(#\x #\y #\z) :test #'char-equal)
                    :h)))))))

#+Test
(pro:with-profiling
  (setf (ds :g-5000-h-projection)
        (project-bipartite-graph
         'undirected-graph-container
         (ds :g-5000)
         :h
         (lambda (v)
           (let ((vertex-class (aref (symbol-name (element v)) 0)))
             (cond ((member vertex-class '(#\a #\b) :test #'char-equal)
                    :m)
                   ((member vertex-class '(#\x #\y #\z) :test #'char-equal)
                    :h)))))))

#+Test
(pro:with-profiling
  (project-bipartite-graph
   'undirected-graph-container
   (ds :g-1000)
   :m
   (lambda (v)
     (let ((vertex-class (aref (symbol-name (element v)) 0)))
       (cond ((member vertex-class '(#\x #\y) :test #'char-equal)
              :m)
             ((member vertex-class '(#\a #\b #\c) :test #'char-equal)
              :h))))))




