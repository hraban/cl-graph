;;; -*- Mode: Lisp; package: cl-user; Syntax: Common-lisp; Base: 10 -*-

(in-package :common-lisp-user)
(defpackage #:cl-graph-test-system (:use #:cl #:asdf))
(in-package #:cl-graph-test-system)

(defsystem cl-graph-test
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Tests for CL-Graph"
  :components ((:module
		"setup"
		:pathname "unit-tests/"
		:components
		((:file "package")
		 (:file "test-graph" :depends-on ("package"))
		 ))
	       (:module 
		"unit-tests"
		:pathname "unit-tests/"
		:depends-on ("setup")
		:components
		((:file "test-graph-container")
		 (:file "test-connected-components")
		 ;;(:file "test-graph-algorithms")
		 (:file "test-api")
		 ))
               
               (:module 
		"dev"
		:components
		((:static-file "notes.text"))))
  :depends-on (:cl-graph :lift))

;; 2008-09-24 - I don't know if this will work or not 
;; i.e., will it happen at the right time wrt everything else
#+asdf-system-connections
(asdf:defsystem-connection cl-graph-test-and-cl-mathstats
  :requires (cl-graph moptilities)
  :components ((:module 
		"unit-tests"
		:components
		((:file "test-graph-metrics")))))
