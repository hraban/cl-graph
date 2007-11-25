;;; -*- Mode: Lisp; package: cl-user; Syntax: Common-lisp; Base: 10 -*-

(in-package #:common-lisp-user)
(defpackage #:cl-graph-system (:use #:cl #:asdf))
(in-package #:cl-graph-system)

(unless (find-system 'asdf-system-connections nil)
  (warn "The CL-Graph system would enjoy having asdf-system-connections 
around. See 
http://www.cliki.net/asdf-system-connections for details and download
instructions."))
(when (find-system 'asdf-system-connections nil)
  (operate 'load-op 'asdf-system-connections))

(defsystem cl-graph
  :version "0.8.4"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Graph manipulation utilities for Common Lisp"
  :components ((:static-file "COPYING")
	       (:module 
		"dev"
		:components 
		((:file "package")
		 (:file "api"
			:depends-on ("package"))
		 (:file "macros"
			:depends-on ("package"))
		 (:file "graph"
			:depends-on ("api" "macros"))
		 (:file "graph-container"
			:depends-on ("graph"))
		 (:file "graph-matrix"
			:depends-on ("graph"))
		 (:file "graph-metrics"
			:depends-on ("graph"))
		 (:file "graph-algorithms"
			:depends-on ("graph"))
                                     
		 (:static-file "notes.text")

		 (:module "graphviz" :depends-on ("graph")
			  :components ((:file "graphviz-support")))))
               (:module 
		"website"
		:components 
		((:module "source"
			  :components ((:static-file "index.lml"))))))
  :in-order-to ((test-op (load-op :cl-graph-test)))
  :perform (test-op :after (op c)
		    (funcall
		      (intern (symbol-name '#:run-tests) :lift)
		      :config :generic))
  :depends-on (:metatilities 
	       :cl-containers
	       :metabang-bind
	       :cl-mathstats
	       ;; makes ASDF-Install get this automatically
	       :asdf-system-connections
	       ))

(defmethod operation-done-p 
           ((o test-op) (c (eql (find-system 'cl-graph))))
  (values nil))

#+asdf-system-connections
(asdf:defsystem-connection cl-graph-and-cl-variates
  :requires (cl-graph cl-variates)
  :components ((:module 
		"dev"
		:components
		((:file "graph-and-variates")
		 (:file "graph-generation"
			:depends-on ("graph-and-variates"))))))

#+asdf-system-connections
(asdf:defsystem-connection cl-graph-and-cl-graphviz
  :requires (cl-graph cl-graphviz)
  :components ((:module 
		"dev"
		:components
		((:module "graphviz"
			  :components
			  ((:file "graphviz-support-optional")))))))

#+asdf-system-connections
(asdf:defsystem-connection cl-graph-and-metacopy
  :requires (cl-graph metacopy)
  :components ((:module 
		"dev"
		:components ((:file "copying")))))
