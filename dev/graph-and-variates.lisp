(in-package cl-user)

#+Ignore
(shadowing-import 
 (list cl-variates:*random-generator*
       cl-variates:random-seed
       cl-variates:integer-random
       cl-variates:uniform-random
       cl-variates:random-boolean
       cl-variates:shuffle-elements!
       cl-variates:random-number-generator
       cl-variates:next-element)
 "CL-GRAPH")

(use-package (find-package "CL-VARIATES") 
             (find-package "CL-GRAPH"))

