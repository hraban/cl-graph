;;; -*- Mode: Lisp; package: asdf-user; Syntax: Common-lisp; Base: 10 -*-

(defsystem "cl-graph"
  :version "0.10.2"
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
		 (:file "graph-algorithms"
			:depends-on ("graph"))
		 #+(or)
		 (:file "dynamic-classes"
			:depends-on ("graph"))
		 (:static-file "notes.text")

		 (:module "graphviz" :depends-on ("graph")
			  :components ((:file "graphviz-support")))))
               (:module
                "website"
		:components
		((:module "source"
			  :components ((:static-file "index.mmd"))))))
  :in-order-to ((test-op (load-op "cl-graph-test")))
  :perform (test-op (op c) (symbol-call :lift :run-tests :config :generic))
  :depends-on ((:version "metatilities-base" "0.6.0")
               (:version "cl-containers" "0.12.0")
               "metabang-bind"))

(load-system "asdf-system-connections")

(defsystem-connection "cl-graph/with-cl-variates"
  :requires ("cl-graph" "cl-variates")
  :components ((:module
		"dev"
		:components
		((:file "graph-and-variates")
		 (:file "graph-generation"
			:depends-on ("graph-and-variates"))))))

(defsystem-connection "cl-graph/with-dynamic-classes"
  :requires ("cl-graph" "dynamic-classes")
  :components ((:module
		"dev"
		:components
		((:file "dynamic-classes")))))

(defsystem-connection "cl-graph/with-cl-graphviz"
  :requires ("cl-graph" "cl-graphviz")
  :components ((:module
		"dev"
		:components
		((:module "graphviz"
			  :components
			  ((:file "graphviz-support-optional")))))))

(defsystem-connection "cl-graph/with-metacopy"
  :requires ("cl-graph" "metacopy")
  :components ((:module
		"dev"
		:components ((:file "copying")))))

(defsystem-connection "cl-graph/with-cl-mathstats"
  :requires ("cl-graph" "cl-mathstats")
  :components ((:module
		"dev"
		:components
		((:file "graph-metrics")))))

(defsystem-connection "cl-graph/with-moptilities"
  :requires ("cl-graph" "moptilities")
  :components ((:module
		"dev"
		:components
		((:file "subgraph-containing")))))




