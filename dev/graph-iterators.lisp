
;;; vertex-iterator

(u:defclass* vertex-iterator (containers::forward-iterator)
  ())


(defmethod initialize-instance :after ((object vertex-iterator) &key)
  (reset object))


(defmethod reset ((iterator vertex-iterator))
  (let ((vertex (containers::initial-container iterator)))
    (setf (slot-value iterator 'containers::iterating-container) 
          (make-iterator (edges vertex)
                         :transform (lambda (e) (other-vertex e vertex)))))
  iterator)


(defmethod containers::base-class-for-iteratee ((container basic-vertex))
  'vertex-iterator)


(defmethod containers::base-class-for-iteratee ((container basic-vertex))
  (containers::base-class-for-iteratee (vertex-edges container)))

(u:add-parameter->dynamic-class :iterator :children nil )

(collect-elements 
