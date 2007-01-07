;;; -*- Mode: Lisp; package: cl-user; Syntax: Common-lisp; Base: 10 -*-

(in-package :common-lisp-user)
(defpackage #:asdf-cl-graph-test (:use #:cl #:asdf))
(in-package #:asdf-cl-graph-test)

(defsystem cl-graph-test
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Tests for CL-Graph"

  :components ((:module 
		"unit-tests"
		:components
		((:file "package")
		 (:file "test-graph" :depends-on ("package"))
		 (:file "test-graph-container" :depends-on ("test-graph"))
		 (:file "test-connected-components" :depends-on ("test-graph"))
		 (:file "test-graph-metrics" :depends-on ("test-graph"))
		 (:file "test-graph-algorithms" :depends-on ("test-graph"))
		 ))
               
               (:module 
		"dev"
		:components
		((:static-file "notes.text"))))
  :depends-on (cl-graph lift))
