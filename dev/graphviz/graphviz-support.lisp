;;;-*- Mode: Lisp; Package: metabang.graph -*-

#| simple-header

$Id: graphviz-support.lisp,v 1.7 2005/06/21 20:51:51 moody Exp $

Copyright 1992 - 2005 Experimental Knowledge Systems Lab, 
University of Massachusetts Amherst MA, 01003-4610
Professor Paul Cohen, Director

Author: Gary King, Levente Mészáros, Attila Lendvai

DISCUSSION

This file contains the stuff that does not depend on cl-graphviz.

|#
(in-package metabang.graph)

;;; ---------------------------------------------------------------------------
;
; This outputs the graph to string in accordance with the DOT file format.  
; For more information about DOT file format, search the web for "DOTTY" and 
; "GRAPHVIZ".
;
(defmethod graph->dot ((g basic-graph) (stream stream)
                       &key 
                       (graph-formatter 'graph->dot-properties)
                       (vertex-key 'vertex-id)
                       (vertex-labeler nil)
                       (vertex-formatter 'vertex->dot)
                       (edge-key nil)
                       (edge-labeler 'princ) 
                       (edge-formatter 'edge->dot))
  (format stream "~A G {~%graph " (if (contains-undirected-edge-p g) "graph" "digraph"))
  (format stream "[")
  (funcall graph-formatter g stream)
  (format stream "];")
  (terpri stream)
  
  ;; vertex formatting
  (iterate-vertexes 
   g
   (lambda (v)
     (terpri stream)
     (let ((key (if vertex-key (funcall vertex-key v) v)))
       (princ key stream)
       (princ " [" stream)
       (when vertex-labeler
         (princ "label=\"" stream)
         (funcall vertex-labeler v stream)
         (princ "\", " stream))
       (funcall vertex-formatter v stream)
       (princ "];" stream))))
  
  (let ((directed-edge-connector (if (contains-undirected-edge-p g) "--" "->"))
        (directed-edge-tag (when (and (contains-undirected-edge-p g)
                                      (contains-directed-edge-p g))
                             "dir=forward, ")))
    (flet ((format-edge (e connector from to directed?)
             (terpri stream)
             (princ (funcall vertex-key from) stream)
             (princ connector stream)
             (princ (funcall vertex-key to) stream) 
             (princ " [" stream)
             (when (and directed? directed-edge-tag)
               (princ directed-edge-tag stream))
             (when edge-key
               (princ "label=\"" stream)
               (funcall edge-labeler e stream)
               (princ "\"," stream))
             (funcall edge-formatter e stream)
             (princ "];" stream)))
      ;; directed edges
      (iterate-vertexes 
       g
       (lambda (v)
         (iterate-target-edges
          v
          (lambda (e) 
            (when (directed-edge-p e)
              (format-edge e directed-edge-connector 
                           (source-vertex e) (target-vertex e) t))))))
      
      ;; undirected edges
      (let ((edges (make-container 'simple-associative-container)))
        (iterate-vertexes 
         g
         (lambda (v)
           (iterate-edges
            v
            (lambda (e)
              (when (and (undirected-edge-p e)
                         (not (item-at-1 edges e)))
                (setf (item-at-1 edges e) t)
                (format-edge e "--" (vertex-1 e) (vertex-2 e) nil)))))))))
  
  (terpri stream)
  (princ "}" stream)
  
  (values g))


#+Test
(let ((g (make-container 'graph-container :default-edge-type :undirected)))
  (loop for (a b) in '((a b) (b c) (b d) (d e) (e f) (d f)) do
        (add-edge-between-vertexes g a b))
  (graph->dot g nil))

#+Test
"graph G {
E []
C []
B []
A []
D []
F []
D--E []
E--F []
B--C []
A--B []
B--D []
D--F []
}"

#+Test
(let ((g (make-container 'graph-container :default-edge-type :directed)))
  (loop for (a b) in '((a b) (b c) (b d) (d e) (e f) (d f)) do
        (add-edge-between-vertexes g a b))
  (graph->dot g nil))

#+Test
"digraph G {
E []
C []
B []
A []
D []
F []
E->F []
B->C []
B->D []
A->B []
D->E []
D->F []
}"

#+Test
(let ((g (make-container 'graph-container)))
  (loop for (a b) in '((d e) (e f) (d f)) do
        (add-edge-between-vertexes g a b :edge-type :directed))
  (loop for (a b) in '((a b) (b c) (b d)) do
        (add-edge-between-vertexes g a b :edge-type :undirected))
  (graph->dot g nil))

#+Test
"graph G {
E []
C []
B []
A []
D []
F []
E--F [dir=forward, ]
D--E [dir=forward, ]
D--F [dir=forward, ]
B--C []
A--B []
B--D []
}"

;;; ---------------------------------------------------------------------------

(defmethod graph->dot ((g basic-graph) (stream (eql nil))
                       &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (with-output-to-string (out)
    (apply #'graph->dot g out args)))

;;; ---------------------------------------------------------------------------

(defmethod graph->dot ((g basic-graph) (stream (eql t))
                       &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (apply #'graph->dot g *standard-output* args))

;;; ---------------------------------------------------------------------------

(defmethod graph->dot ((g basic-graph) (stream string)
                       &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (with-open-file (out stream :direction :output :if-exists :supersede)
    (apply #'graph->dot g out args)))

;;; ---------------------------------------------------------------------------

(defmethod graph->dot ((g basic-graph) (stream pathname)
                       &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (apply #'graph->dot g (namestring stream) args))

;;; ---------------------------------------------------------------------------

(defmethod graph->dot-properties ((g t) (stream t))
  (values))

;;; ---------------------------------------------------------------------------

(defmethod vertex->dot ((v basic-vertex) (stream stream))
  (values))

;;; ---------------------------------------------------------------------------

(defmethod edge->dot ((v basic-edge) (stream stream))
  (values))

;;; ---------------------------------------------------------------------------
;;; dot->graph
;;; ---------------------------------------------------------------------------

#|
(defmethod dot->graph ((dot-stream stream)
                       &key)
  )

;;; ---------------------------------------------------------------------------

(defmethod dot->graph ((dot-stream string)
                       &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (with-open-file (out stream :direction :output :if-exists :supersede)
    (apply #'dot->graph g out args)))

;;; ---------------------------------------------------------------------------

(defmethod dot->graph ((dot-stream pathname)
                       &rest args &key &allow-other-keys)
  (declare (dynamic-extent args))
  (with-open-file (out stream :direction :output :if-exists :supersede)
    (apply #'dot->graph g out args))
  (apply #'dot->graph g (namestring stream) args))

|#

(defparameter *dot-graph-attributes*
  '((:size coordinate)
    (:bb bounding-box)
    (:page text)
    (:ratio (:fill :compress :auto)) ;; Could actually be a float number too
    (:margin float)
    (:nodesep float)
    (:ranksep float)
    (:ordering (:out))
    (:rankdir ("LR" "RL" "BT"))
    (:pagedir text)
    (:rank (:same :min :max))
    (:rotate integer)
    (:center integer)
    (:nslimit float)
    (:mclimit float)
    (:layers text)
    (:color text)
    (:bgcolor text)))

(defparameter *dot-vertex-attributes*
  '((:pos coordinate)
    (:height float)
    (:width float)
    (:fixed-size boolean)
    (:label text)
    (:shape (:record :plaintext :ellipse :circle :egg :triangle :box
             :diamond :trapezium :parallelogram :house :hexagon :octagon
             :doublecircle))
    (:fontsize integer)
    (:fontname text)
    (:fontcolor text)
    (:color text)
    (:fillcolor text)
    (:style (:filled :solid :dashed :dotted :bold :invis))
    (:layer text)
    (:url text)))

(defparameter *dot-edge-attributes*
  '((:pos spline)
    (:minlen integer)
    (:weight integer)
    (:label text)
    (:fontsize integer)
    (:fontname text)
    (:fontcolor text)
    (:style (:solid :dashed :dotted :bold :invis))
    (:color text)
    (:dir (:forward :back :both :none))
    (:tailclip boolean)
    (:headclip boolean)
    (:arrowhead (:none :normal :inv :dot :odot :invdot :invodot :tee
                 :empty :invempty :open :halfopen :diamond :odiamond
                 :box :obox :crow))
    (:arrowtail (:none :normal :inv :dot :odot :invdot :invodot :tee
                 :empty :invempty :open :halfopen :diamond :odiamond
                 :box :obox :crow))
    (:headlabel text)
    (:taillabel text)
    (:labelfontsize integer)
    (:labelfontname text)
    (:labelfontcolor text)
    (:labeldistance integer)
    (:port-label-distance integer)
    (:decorate boolean)
    (:samehead boolean)
    (:sametail boolean)
    (:constraint boolean)
    (:layer text)))

(defclass* dot-attributes-mixin ()
  ((dot-attributes nil ia))
  (:export-p t))

(defclass* dot-graph-mixin (dot-attributes-mixin) ()
  (:export-p t)
  (:default-initargs
    :vertex-class 'dot-vertex
    :directed-edge-class 'dot-directed-edge
    :undirected-edge-class 'dot-edge))
(defclass* dot-vertex-mixin (dot-attributes-mixin) ()
  (:export-p t))
(defclass* dot-edge-mixin (dot-attributes-mixin) ()
  (:export-p t))

(defclass* dot-graph (dot-graph-mixin graph-container)
  ()
  (:export-p t))

(defclass* dot-vertex (dot-vertex-mixin graph-container-vertex) ()
  (:export-p t))
(defclass* dot-edge (dot-edge-mixin graph-container-edge) ()
  (:export-p t))
(defclass* dot-directed-edge (dot-edge directed-edge-mixin) ()
  (:export-p t))


(defmethod (setf dot-attribute-value) :before (value (attr symbol) (thing dot-attributes-mixin))
  (declare (ignore value))
  (ensure-valid-dot-attribute attr thing))

(defmethod (setf dot-attribute-value) (value (attr symbol) (thing dot-attributes-mixin))
  (setf (getf (dot-attributes thing) attr) value))

(defmethod dot-attribute-value ((attr symbol) (thing dot-attributes-mixin))
  (getf (dot-attributes thing) attr))

(defmethod graph->dot-properties ((graph dot-graph-mixin) (stream t))
  (loop for (name value) on (dot-attributes graph) by #'cddr
        do
        (print-dot-key-value name value *dot-graph-attributes* stream)))

(defmethod vertex->dot ((vertex dot-vertex-mixin) (stream t))
  (format-dot-attributes vertex *dot-vertex-attributes* stream))

(defmethod edge->dot ((edge dot-edge-mixin) (stream t))
  (format-dot-attributes edge *dot-edge-attributes* stream))

(defun format-dot-attributes (object dot-attributes stream)
  (loop for (name value) on (dot-attributes object) by #'cddr
        for prefix = "" then ", " do
        (write-string prefix stream)
        (print-dot-key-value name value dot-attributes stream)))

(defmethod ensure-valid-dot-attribute (key (object dot-graph-mixin))
  (or (assoc key *dot-graph-attributes*)
      (error "Invalid dot graph attribute ~S" key)))

(defmethod ensure-valid-dot-attribute (key (object dot-vertex-mixin))
  (or (assoc key *dot-vertex-attributes*)
      (error "Invalid dot vertex attribute ~S" key)))

(defmethod ensure-valid-dot-attribute (key (object dot-edge-mixin))
  (or (assoc key *dot-edge-attributes*)
      (error "Invalid dot edge attribute ~S" key)))

(defun print-dot-key-value (key value dot-attributes stream)
  (destructuring-bind (key value-type)
      (or (assoc key dot-attributes)
          (error "Invalid attribute ~S" key))
    (write-name-for-dot key stream)
    (format stream "=~a" 
            (etypecase value-type
              ((member coordinate)
               (with-output-to-string (str)
                 (princ "\"" str)
                 (let ((first t))
                   (dolist (el value)
                     (unless first
                       (princ "," str))
                     (princ el str)
                     (setf first nil)))
                 (princ "\"" str)))
              ((member spline)
               (with-output-to-string (str)
                 (princ "\"" str)
                 (let ((first t))
                   (dolist (el value)
                     (unless first
                       (princ " " str))
                     (princ (first el) str)
                     (princ "," str)
                     (princ (second el) str)
                     (setf first nil)))
                 (princ "\"" str)))
              ((member bounding-box)
               (with-output-to-string (str)
                 (princ "\"" str)
                 (let ((first t))
                   (dolist (el value)
                     (unless first
                       (princ ", " str))
                     (princ (first el) str)
                     (princ "," str)
                     (princ (second el) str)
                     (setf first nil)))
                 (princ "\"" str)))
              ((member integer)
               (unless (typep value 'integer)
                 (error "Invalid value for ~S: ~S is not an integer"
                        key value))
               value)
              ((member boolean)
               (if value
                   "true"
                   "false"))
              ((member text)
               (textify value))
              ((member float)
               (coerce value 'single-float))
              (list
               (unless (member value value-type :test 'equal)
                 (error "Invalid value for ~S: ~S is not one of ~S"
                        key value value-type))
               (if (symbolp value)
                   (string-downcase value)
                   value))))))

(defmethod write-name-for-dot (attribute stream)
  (format stream "~(~A~)" attribute))

(defmethod write-name-for-dot ((attribute (eql :url)) stream)
  (format stream "URL"))

(defun textify (object)
  (let ((string (princ-to-string object)))
    (with-output-to-string (stream)
      (write-char #\" stream)
      (loop for c across string do
            ;; Note: #\\ should not be escaped to allow \n, \l, \N, etc.
            ;; to work.
            (case c
              ((#\")
               (write-char #\\ stream)
               (write-char c stream))
              (#\Newline
               (write-char #\\ stream)
               (write-char #\n stream))
              (t
               (write-char c stream))))
      (write-char #\" stream))))

;;; ---------------------------------------------------------------------------
;
; Calls the dot executable to create external output for graphs
;
#+(or win32 mswindows)
(defvar *dot-path* "\"C:/Program Files/ATT/Graphviz/bin/dot.exe\"")
#+(or linux unix)
(defvar *dot-path* "/usr/bin/dot" "Path to `dot`")

(defmethod graph->dot-external ((g basic-graph) file-name &key (type :ps))
  "Generate an external represenation of a graph to a file, by running
the program in *dot-path*."
  (let ((dot-string (graph->dot g nil))
        (dot-type (concatenate 'string "-T" (string-downcase (symbol-name type)))))
    #+lispworks (with-open-stream
                    (s (sys:open-pipe (concatenate 'string *dot-path* " -Tpng -o" file-name)
                                      :direction :input))
                    (write-line dot-string s)
                    (force-output s)
		    (close s))
    #+sbcl
    (sb-ext:run-program *dot-path*
                        (list dot-type "-o" file-name)
                        :input (make-string-input-stream dot-string)
                        :output *standard-output*)
    #-(or sbcl lispworks)
    (error "Don't know how to execute a program on this platform")))

;;; ---------------------------------------------------------------------------
;
; Test dot external
;
(defun test-dot-external ()
  (let* ((g (make-graph 'dot-graph))
         (v1 (add-vertex g 'a :dot-attributes '(:shape :box
                                                :color :blue)))
         (v2 (add-vertex g 'b :dot-attributes '(:shape :circle
                                                :style :filled
                                                :color :yellow))))
    (add-edge-between-vertexes g v1 v2
                               :dot-attributes '(:arrowhead :open
                                                 :arrowtail :normal
                                                 :style :dotted))
    (print (graph->dot g nil))
    (graph->dot-external g "/tmp/test.gif" :type :gif)))
