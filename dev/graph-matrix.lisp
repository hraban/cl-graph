;;;-*- Mode: Lisp; Package: metabang.graph -*-

#| simple-header

$Id: graph-matrix.lisp,v 1.1 2005/05/01 21:40:26 gwking Exp $

Copyright 1992 - 2005 Experimental Knowledge Systems Lab, 
University of Massachusetts Amherst MA, 01003-4610
Professor Paul Cohen, Director

Author: Gary King

DISCUSSION

CtF uses an adj list (vector of edges with lists) or adj matrix (vector with vectors)

I think I'd like a numeric class and then a object one... maybe someday
|#
(in-package #:metabang.graph)

;;; ---------------------------------------------------------------------------

(defclass* graph-matrix (basic-graph)
  ((adjencency-matrix nil r))
  (:default-initargs
    :vertex-class 'graph-matrix-vertex
    :undirected-edge-class 'graph-matrix-edge)
  (:export-p t)
  (:documentation "Stub for matrix based graph. Not implemented."))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object graph-matrix) &key)
  (setf (slot-value object 'adjencency-matrix) 
        nil))

;;; ---------------------------------------------------------------------------

(defmethod make-vertex-container ((graph graph-matrix) initial-size) 
  (make-container 'vector-container :initial-size initial-size
                  :fill-pointer 0))

;;; ---------------------------------------------------------------------------

(defmethod make-edge-container ((graph graph-matrix) initial-size) 
  (make-container 'vector-container :initial-size initial-size
                  :fill-pointer 0))

;;; ---------------------------------------------------------------------------

(defclass* graph-matrix-edge (basic-edge)
  ()
  #+COPYING :copy-slots
  (:export-p t)
  (:documentation "Stub for matrix based graph. Not implemented."))

;;; ---------------------------------------------------------------------------

(defclass* graph-matrix-vertex (basic-vertex)
  ()
  (:export-p t)
  (:documentation "Stub for matrix based graph. Not implemented."))

;;; ---------------------------------------------------------------------------

