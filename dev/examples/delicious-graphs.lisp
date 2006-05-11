(in-package #:metatilities)

#|
color by tag weight
|#

(defclass* delicious-post ()
  ((post-time nil ia :initarg :time)
   (tags nil ia :initarg :tag)
   (hash nil ia)
   (extended nil ia)
   (description nil ia)
   (post-href nil ia :initarg :href)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object delicious-post) &key)
  (setf (tags object) (make-tags-canonical (tags object))))

;;; ---------------------------------------------------------------------------
  
(defgeneric make-tags-canonical (tags)
  (:documentation "Help convert del.icio.us tags into a canonicl form."))

;;; ---------------------------------------------------------------------------

(defgeneric make-tag-canonical (tag)
  (:documentation "Help convert del.icio.us tags into a canonicl form."))

;;; ---------------------------------------------------------------------------

(defmethod make-tags-canonical ((tags list))
  (mapcar #'make-tag-canonical tags))

;;; ---------------------------------------------------------------------------

(defmethod make-tags-canonical ((tags string))
  (make-tags-canonical (tokenize-string tags :delimiter #\ )))

;;; ---------------------------------------------------------------------------

(defmethod make-tag-canonical ((tag symbol))
  tag)

;;; ---------------------------------------------------------------------------

(defmethod make-tag-canonical ((tag string))
  (form-keyword (string-upcase tag)))

;;; ---------------------------------------------------------------------------

(defun determine-tag-counts (delicious-post-file)
  "Returns a list of tags and their counts from a delicious-post-file."
  (bind ((posts (xmls::parse delicious-post-file))
         (tags (collect-elements 
                ;; the first two elements of posts aren't tags
                (cddr posts)
                :transform
                (lambda (post-info)
                  (let ((tags (find "tag" (second post-info) 
                                    :test #'string-equal
                                    :key #'first)))
                    (when tags 
                      (tokenize-string (second tags) :delimiter #\ )))))))
    (element-counts 
     (flatten tags)
     :test #'equal
     :sort #'>
     :sort-on :counts)))

#+Example
;; this is what a post looks like after it's been transformed by xmls
("post"
 (("time" "2005-11-21T15:25:47Z")
  ("tag" "yoga health exercise amherst")
  ("hash" "9aad47baf972813c8202b43a56e95a61")
  ("description" "Yoga Center Amherst, Massachusetts")
  ("href" "http://www.yogacenteramherst.com/")))

(defun parse-delicious-posts (delicious-post-file)
  "Transform a delicious post file into a list of post objects."
  (collect-elements
   (cddr (xmls::parse delicious-post-file))
   :transform
   (lambda (post-info)
     (apply #'make-instance
            'delicious-post
            (loop for (name value) in (second post-info) nconc
                  (list (form-keyword (string-upcase name)) value)))))) 

;;; ---------------------------------------------------------------------------

(defun create-bipartite-tag/post-graph (delicious-post-file)
  "Creates a bipartite graph of tags, posts and the links between them from 
a delicious post file."
  (bind ((posts (parse-delicious-posts delicious-post-file))
         (g (cl-graph:make-graph 'cl-graph:graph-container)))
    (iterate-elements 
     posts
     (lambda (post)
       (iterate-elements 
        (tags post)
        (lambda (tag)
          (cl-graph:add-edge-between-vertexes g post tag)))))
    g))

;;; ---------------------------------------------------------------------------

#+Example
;; all tags
(cl-graph:graph->dot
 (cl-graph:project-bipartite-graph 
  (cl-graph:make-graph 'cl-graph:graph-container 
                       :default-edge-class 'cl-graph:weighted-edge)
  (create-bipartite-tag/post-graph #P"user-home:temporary;all-posts.xml")
  'keyword
  (compose 'type-of 'element))
 "user-home:temporary;all-tags.dot"
 :vertex-labeler (lambda (vertex stream)
                   (format stream "~(~A~)" (symbol-name (element vertex))))
 :edge-formatter (lambda (edge stream)
                   (format stream "weight=~D" (cl-graph:weight edge))))

;;; ---------------------------------------------------------------------------
 
#+Example
(cl-graph:graph->dot
 (cl-graph:make-filtered-graph
  (cl-graph:project-bipartite-graph 
   (cl-graph:make-graph 'cl-graph:graph-container 
                        :default-edge-class 'cl-graph:weighted-edge)
   (create-bipartite-tag/post-graph #P"user-home:temporary;all-posts.xml")
   'keyword
   (compose 'type-of 'element))
  (lambda (v)
    (search "lisp" (symbol-name (element v)) :test #'string-equal))
  :complete-closure-with-links
  1)
 "user-home:temporary;lisp-tags-20051125.dot"
 :vertex-labeler (lambda (vertex stream)
                   (format stream "~(~A~)" (symbol-name (element vertex))))
 :edge-formatter (lambda (edge stream)
                   (format stream "weight=~D" (cl-graph:weight edge))))

