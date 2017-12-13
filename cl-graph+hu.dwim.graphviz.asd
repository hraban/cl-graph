;;; -*- Mode: Lisp; package: asdf-user; Syntax: Common-lisp; Base: 10 -*-

(defsystem "cl-graph+hu.dwim.graphviz"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Integration of cl-graph with hu.dwim.graphviz"
  :components ((:module "dev"
                :components
                ((:module "graphviz"
                  :components
                  ((:file "graphviz-support-optional"))))))
  :depends-on ("cl-graph" "hu.dwim.graphviz"))
