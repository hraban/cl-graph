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

