;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

#| simple-header

Author: Gary King, et. al.

DISCUSSION

|#
(in-package common-lisp-user)

(defpackage "CL-GRAPH"
  (:use "COMMON-LISP" "METATILITIES" "CL-CONTAINERS" 
        "METABANG.BIND" "METABANG.MATH")
  (:nicknames "METABANG.GRAPH")
  (:documentation "CL-Graph is a Common Lisp library for manipulating graphs and running graph algorithms.")
  
  (:export 
   #:with-changing-vertex
   
   #:make-graph
   #:basic-graph
   
   #:add-edge-between-vertexes     ; graph { value | vertex } { value | vertex }
   #:delete-edge-between-vertexes  ; graph { value | vertex } { value | vertex }
   #:add-vertex                    ; graph { value | vertex }
   #:find-vertex                   ; graph { value | vertex }
   #:find-edge                     ; graph edge
   #:find-edge-between-vertexes    ; graph { vertex | value } { vertex | value }
   #:find-vertex-if
   #:find-vertexes-if
   #:search-for-vertex
   
   #:iterate-container             ; graph fn
   #:iterate-vertexes
   #:vertexes
   #:source-edges
   #:target-edges
   #:child-vertexes
   #:parent-vertexes
   #:neighbor-vertexes
   #:other-vertex
   
   #:edge-count                    ; graph
   #:vertex-count                  ; graph
   #:source-edge-count             ; vertex
   #:target-edge-count             ; vertex
   
   #:rootp                         ; vertex
   #:graph-roots                   ; graph
   
   #:topological-sort              ; graph
   #:depth                         ; graph | vertex
   #:depth-level
   
   #:get-transitive-closure        ;; CTM
   #:make-filtered-graph           ;; CTM
   
   #:adjacentp
   #:in-cycle-p                    ; graph vertex
   #:force-undirected
   
   #:renumber-vertexes
   #:renumber-edges
   
   #:generate-directed-free-tree
   
   #:contains-undirected-edge-p
   #:contains-directed-edge-p
   
   #:undirected-edge-p
   #:directed-edge-p
   #:tagged-edge-p
   #:untagged-edge-p
   #:tag-all-edges
   #:untag-all-edges
   #:graph->dot
   #:vertex->dot
   #:edge->dot
   #:graph->dot-properties
   #:subgraph-containing
   #:graph->dot-external
   #:dot-graph
   #:dot-vertex
   #:dot-edge
   #:dot-attributes
   
   #:connected-graph-p
   #:find-connected-components
   #:connected-component-count
   
   #:target-vertex
   #:source-vertex
   
   #:layout-graph-with-graphviz
   #:dot-attribute-value
   #:dot-attribute

   #:add-edge                      ; graph edge
   #:delete-edge                   ; graph edge
   
   #:add-vertex                    ; graph { value | vertex }
   #:delete-vertex                 ; graph { value | vertex }
   #:find-vertex                   ; graph { value | vertex }
   #:find-edge                     ; graph edge
   #:find-edge-between-vertexes    ; graph { vertex | value } { vertex | value }
   #:find-edge-between-vertexes-if ; graph { vertex | value } { vertex | value } fn
   #:find-edge-if                  ; graph
   #:find-edges-if                 ; graph
   
   #:edges                         ; graph | vertex
   #:iterate-edges                 ; graph fn
   #:iterate-source-edges          ; vertex fn
   #:iterate-target-edges          ; vertex fn
   #:iterate-children              ; vertex (nodes) fn
   #:iterate-parents               ; vertex (nodes) fn
   #:iterate-neighbors             ; vertex (all neighbors) fn
   #:has-children-p
   #:has-parent-p
   #:number-of-neighbors
   
   #:edge-count                    ; graph
   #:vertex-count                  ; graph
   
   #:topological-sort              ; graph
   #:depth                         ; graph | vertex
   #:depth-level
   
   #:get-transitive-closure        ;; CTM
   #:make-filtered-graph           ;; CTM
   
   #:adjacentp
   #:in-cycle-p                    ; graph vertex
   #:in-undirected-cycle-p         ; graph vertex
   #:any-undirected-cycle-p        ; graph
   #:force-undirected
   #:vertices-share-edge-p
   
   #:map-paths
   #:map-shortest-paths
   
   ;;; depth first search 
   #:dfs-edge-type
   #:dfs-back-edge-p 
   #:dfs-tree-edge-p
   #:edge-lessp-by-direction
   #:out-edge-for-vertex-p
   #:dfs
   
   ;;; minimum-spanning-tree
   #+Ignore #:add-edges-to-graph
   
   #:make-graph-from-vertexes
   #:edge-lessp-by-weight
   #:minimum-spanning-tree
   
   ;;; mapping
   #+Ignore #:map-over-all-combinations-of-k-vertexes
   #+Ignore #:map-over-all-combinations-of-k-edges
   
   #:project-bipartite-graph
   
   #:make-vertex-edges-container 
   
   #:vertex-degree-counts
   #:vertex-degree
   #:average-vertex-degree
   #:vertex-clustering-coefficient
   #:average-vertex-clustering-coefficient
   
   #:graph-mixing-matrix
   #:graph-edge-mixture-matrix
   #:assortativity-coefficient
   #:vertex-degree-summary))