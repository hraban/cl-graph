(in-package cl-graph)

(metacopy:defcopy-methods basic-vertex 
  :copy-all t)

(metacopy:defcopy-methods basic-edge 
  :set (edge-id tag color graph)
  :copy (element))

(metacopy:defcopy-methods weighted-edge-mixin 
  :copy-all t)

(metacopy:defcopy-methods basic-graph 
  :copy-all t)

(metacopy:defcopy-methods graph-container-vertex 
  :copy-all t)

(metacopy:defcopy-methods graph-container-edge 
  :copy-all t)

(defmethod generate-directed-free-tree ((graph basic-graph) (root basic-vertex))
  (let ((new-graph (metacopy:copy-thing graph)))
    (empty! new-graph)
    (nilf (contains-undirected-edge-p new-graph))
    (neighbors-to-children new-graph root)
    (values new-graph)))