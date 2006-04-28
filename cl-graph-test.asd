;;; -*- Mode: Lisp; package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

#|

|#

(in-package :common-lisp-user)
(defpackage #:asdf-cl-graph-test (:use #:cl #:asdf))
(in-package #:asdf-cl-graph-test)

(defsystem cl-graph-test
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Tests for CL-Graph"

  :components ((:module "unit-tests"
                        :components ((:file "package")
                                     (:file "test-graph" :depends-on ("package"))
                                     (:file "test-graph-container" :depends-on ("test-graph"))
                                     (:file "test-connected-components" :depends-on ("test-graph"))
                                     (:file "test-graph-metrics" :depends-on ("test-graph"))
                                     (:file "test-graph-algorithms" :depends-on ("test-graph"))
                                     ))
               
               (:module "dev"
                        :components ((:static-file "notes.text"))))
  
  :in-order-to ((test-op (load-op cl-graph-test)))
  :depends-on (cl-graph lift))

;;; ---------------------------------------------------------------------------

(defmethod perform :after ((op test-op ) (c (eql (find-system 'cl-graph-test))))
  (describe
   (funcall 
    (intern (symbol-name '#:run-tests) '#:lift) 
    :suite (intern (symbol-name '#:cl-graph-test) '#:cl-graph-test))))

;;; ---------------------------------------------------------------------------

(defmethod perform :after ((o load-op) (c (eql (find-system 'cl-graph-test))))
  )

(defmethod operation-done-p ((o test-op) (c (eql (find-system 'cl-graph-test))))
  ;; testing is never done...
  (values nil))
