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
                                     (:file "test*" :depends-on ("package"))))
               
               (:module "dev"
                        :components ((:static-file "notes.text"))))
  
  :in-order-to ((test-op (load-op moptilities-test)))
  
  :perform (test-op :after (op c)
                    (describe
                     (funcall 
                      (intern (symbol-name '#:run-tests) '#:lift) 
                      :suite (intern (symbol-name '#:cl-graph-test) '#:cl-graph-test))))
  :depends-on (cl-graph lift))

;;; ---------------------------------------------------------------------------

(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system 'moptilities-test))))
  (values nil))
