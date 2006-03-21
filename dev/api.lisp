(in-package cl-graph)

;;; ---------------------------------------------------------------------------

(defgeneric make-vertex-container (graph initial-size)
  (:documentation "Make-vertex-container is called during graph creation and can be used to create specialized containers to hold graph vertexes."))

;;; ---------------------------------------------------------------------------

(defgeneric make-edge-container (graph initial-size)
  (:documentation "Make-edge-container is called during graph creation and can be used to create specialized containers to hold graph edges."))

;;; ---------------------------------------------------------------------------
;;; API
;;; ---------------------------------------------------------------------------

(defgeneric make-graph (graph-type &key)
  (:documentation "Create a new graph of type `graph-type'. Graph type can be 
a symbol naming a sub-class of basic-graph or a list. If it is a list of symbols naming
different classes. If graph-type is a list, then a class which has all of the listed 
classes as superclasses will be found (or created). In either case, the new graph will
be created as if with a call to make-instance."))

;;; ---------------------------------------------------------------------------

(defgeneric make-edge-for-graph (graph vertex-1 vertex-2 
                                       &key edge-type edge-class &allow-other-keys)
  (:documentation "It should not usually necessary to call this in user code. Creates a new edge between vertex-1 and vertex-2 for the graph. If the edge-type and edge-class are not specified, they will be determined from the defaults of the graph."))

;;; ---------------------------------------------------------------------------

(defgeneric add-edge (graph edge &rest args &key force-new?)
  (:documentation "Add-edge adds an existing edge to a graph. As add-edge-between-vertexes is generally more natural to use, this method is rarely called."))

;;; ---------------------------------------------------------------------------

(defgeneric add-edge-between-vertexes (graph value-or-vertex-1 value-or-vertex-2
                                              &rest args &key if-duplicate-do
                                              edge-type)
  (:documentation "Adds an edge between two vertexes and returns it.  
If force-new? is true, 
the edge is added even if one already exists. 
If the vertexes are not 
found in the graph, they will be added \(unless :error-if-not-found? is
true\). The class of the edge can be specified using :edge-class or
:edge-type. If :edge-type is used, it can be either :directed or 
:undirected; the actual class of the edge will be determined by using
the edge-types of the graph. If neither :edge-type nor :edge-class is
specified, then a directed edge will be created.

If-duplicate-do is used when the 'same' edge is added more than once. It can be
either a function on one variable or :ignore or :force. If it is :ignore, then
the previously added edge is returned; if it is :force, then another edge is
added between the two vertexes; if it is a function, then this function will
be called with the previous edge."))

;;; ---------------------------------------------------------------------------

(defgeneric delete-edge (graph edge)
  (:documentation "Delete the `edge' from the `graph' and returns it."))

;;; ---------------------------------------------------------------------------

(defgeneric delete-edge-between-vertexes (graph value-or-vertex-1
                                                value-or-vertex-2 &rest args)
  (:documentation "Finds an edge in the graph between the two specified vertexes. If values (i.e., non-vertexes) are passed in, then the graph will be searched for matching vertexes."))

;;; ---------------------------------------------------------------------------

(defgeneric add-vertex (graph value-or-vertex &key if-duplicate-do)
  (:documentation  "Adds a vertex to a graph. If called with a vertex, then this vertex is added. If called with a value, then a new vertex is created to hold the value. If-duplicate-do can be one of :ignore, :force, :replace, :replace-value or a function. The default is :ignore."))

;;; ---------------------------------------------------------------------------

(defgeneric delete-vertex (graph value-or-vertex)
  (:documentation "Remove a vertex from a graph. The 'vertex-or-value' argument can be
a vertex of the graph or a 'value' that will find a vertex via a call to find-vertex. A
graph-vertex-not-found-error will be raised if the vertex is not found or is not part of
the graph."))

;;; ---------------------------------------------------------------------------

(defgeneric find-vertex (graph value &optional error-if-not-found?)
  (:documentation "Search 'graph' for a vertex with element 'value'. The search is fast but inflexible because it uses an associative-container. If you need more flexibity, see search-for-vertex."))

;;; ---------------------------------------------------------------------------

(defgeneric search-for-vertex (graph value &key key test error-if-not-found?)
  (:documentation "Search 'graph' for a vertex with element 'value'. The 'key' function is applied to each element before that element is compared with the value. The comparison is done using the function 'test'. If you don't need to use key or test, then consider using find-vertex instead."))

;;; ---------------------------------------------------------------------------

(defgeneric find-edge (graph edge &optional error-if-not-found?)
  (:documentation "Search `graph` for an edge whose vertexes match `edge`. This means that `vertex-1` of the edge in the graph must match `vertex-1` of `edge` and so forth. Wil signal an error of type `graph-edge-not-found-error` unless `error-if-not-found?` is nil. [?? Unused. Remove?]"))

;;; ---------------------------------------------------------------------------

(defgeneric find-edge-between-vertexes (graph value-or-vertex-1 value-or-vertex-2
                                              &key error-if-not-found?)
  (:documentation "Searches `graph` for an edge that connects vertex-1 and vertex-2.  [?? Ignores error-if-not-found? Does directedness matter? need test]"))

;;; ---------------------------------------------------------------------------

(defgeneric source-vertex (edge)
  (:documentation "Returns the source-vertex of a directed edge. Compare with `vertex-1`."))

;;; ---------------------------------------------------------------------------

(defgeneric target-vertex (edge)
  (:documentation "Returns the target-vertex of a directed edge. Compare with `vertex-2`."))

;;; ---------------------------------------------------------------------------

(defgeneric iterate-edges (graph-or-vertex fn)
  (:documentation "Calls `fn` on each edge of graph or vertex."))

;;; ---------------------------------------------------------------------------

(defgeneric iterate-source-edges (vertex fn)
  (:documentation "In a directed graph, calls `fn` on each edge of a vertex that begins at vertex. In an undirected graph, this is equivalent to `iterate-edges`."))

;;; ---------------------------------------------------------------------------

(defgeneric iterate-target-edges (vertex fn)
  (:documentation "In a directed graph, calls `fn` on each edge of a vertex that ends at vertex. In an undirected graph, this is equivalent to `iterate-edges`."))

;;; ---------------------------------------------------------------------------

(defgeneric iterate-children (vertex fn)
  (:documentation "Calls fn on every vertex that is either connected to vertex by an undirected edge or is at the target end of a directed edge."))

;;; ---------------------------------------------------------------------------

(defgeneric has-children-p (vertex)
  (:documentation "In a directed graph, returns true if vertex has any edges that point from vertex to some other vertex (cf. iterate-target-edges). In an undirected graph, `has-children-p` is testing only whether or not the vertex has any edges."))

;;; ---------------------------------------------------------------------------

(defgeneric has-parent-p (vertex)
  (:documentation "In a directed graph, returns true if vertex has any edges that point from some other vertex to this vertex (cf. iterate-source-edges). In an undirected graph, `has-parent-p` is testing only whether or not the vertex has any edges."))

;;; ---------------------------------------------------------------------------

(defgeneric iterate-parents (vertex fn)
  (:documentation "Calls fn on every vertex that is either connected to vertex by an undirected edge or is at the source end of a directed edge."))

;;; ---------------------------------------------------------------------------

(defgeneric iterate-neighbors (vertex fn)
  (:documentation "Calls fn on every vertex adjecent to vertex See also iterate-children and iterate-parents."))

;;; ---------------------------------------------------------------------------

(defgeneric renumber-vertexes (graph)
  (:documentation "Assign a number to each vertex in a graph in some unspecified order. [?? internal]"))

;;; ---------------------------------------------------------------------------

(defgeneric renumber-edges (graph)
  (:documentation "Assign a number to each edge in a graph in some unspecified order. [?? internal]"))

;;; ---------------------------------------------------------------------------

(defgeneric generate-directed-free-tree (graph root)
  (:documentation "Returns a version of graph which is a directed free tree
rooted at root."))

;;; ---------------------------------------------------------------------------

(defgeneric in-undirected-cycle-p (graph start-vertex &optional marked previous)
  (:documentation "Return true if-and-only-if an undirected cycle in graph is reachable from start-vertex."))

;;; ---------------------------------------------------------------------------

(defgeneric undirected-edge-p (edge)
  (:documentation "Returns true if-and-only-if edge is undirected"))

;;; ---------------------------------------------------------------------------

(defgeneric directed-edge-p (edge)
  (:documentation "Returns true if-and-only-if edge is directed"))

;;; ---------------------------------------------------------------------------

(defgeneric tagged-edge-p (edge)
  (:documentation "Returns true if-and-only-if edge's tag slot is t"))

;;; ---------------------------------------------------------------------------

(defgeneric untagged-edge-p (edge)
  (:documentation "Returns true if-and-only-if edge's tage slot is nil"))
          
;;; ---------------------------------------------------------------------------

(defgeneric adjacentp (graph vertex-1 vertex-2)
  (:documentation "Return true if vertex-1 and vertex-2 are connected by an edge. [?? compare with vertices-share-edge-p and remove one or maybe call one directed-adjacentp]"))

;;; ---------------------------------------------------------------------------

(defgeneric make-filtered-graph (old-graph test-fn &optional graph-completion-method depth)
  (:documentation "Takes a GRAPH and a TEST-FN (a single argument function
returning NIL or non-NIL), and filters the graph nodes according to 
the test-fn (those that return non-NIL are accepted), returning 
a new graph with only nodes corresponding to those in the 
original graph that satisfy the test (the nodes in the new graph 
are new, but their values and name point to the same contents of 
the original graph).  There are four options for how the new 
graph is filled-out, depending on the following keywords passed 
to the optional GRAPH-COMPLETION-METHOD argument:

*  NIL (default)    

     New graph has only nodes that correspond to those in
       the original graph that pass the test.  NO LINKS are
       reproduced.

*  :COMPLETE-LINKS  

     New graph has only nodes that pass, but reproduces 
       corresponding links between passing nodes in the
       original graph.

*  :COMPLETE-CLOSURE-NODES-ONLY

     New graph also includes nodes corresponding to the 
       transitive closure(s) that include the passign nodes 
       in the original graph.  NO LINKS are reproduced.

*  :COMPLETE-CLOSURE-WITH-LINKS

     Same as above, except corresponding links are reproduced.

For both transitive closure options, an additional optional argument,
DEPTH, specifies how many links away from a source vertex to travel 
in gathering vertexes of the closure.  E.g., a depth of 1 returns the 
source vertex and the parents and children of that vertex (all vertexes
one link away from the source).  The default value is NIL, indicating
that all vertexes are to be included, no matter their depth.  This
value is ignored in non closure options."))

;;; ---------------------------------------------------------------------------

(defgeneric project-bipartite-graph  
  (new-graph existing-graph vertex-class vertex-classifier)
  (:documentation "Creates the unimodal bipartite projects of existing-graph with
vertexes for each vertex of existing graph whose `vertex-classifier` is eq to `vertex-class` and where an edge existing between two vertexes of the graph if and only if they are connected to a shared vertex in the existing-graph."))

;;; ---------------------------------------------------------------------------

(defgeneric assortativity-coefficient (mixing-matrix)
  (:documentation "An assortative graph is one where vertexes of the same type are more likely to 
have edges between them. The \(discrete\) assortativity-coefficient measures how
assortative a graph is based on its mixing matrix. The definition we use is from 
Mixing Patterns in Networks by Mark Newman. See the citation 'newman200-mixing' in moab 
or the URL 'http://arxiv.org/abs/cond-mat/0209450'."))

;;; ---------------------------------------------------------------------------

(defgeneric graph->dot (graph output
                       &key 
                       graph-formatter
                       vertex-key
                       vertex-labeler
                       vertex-formatter
                       edge-key
                       edge-labeler 
                       edge-formatter)
  (:documentation 
   "Generates a description of `graph` in DOT file format. The formatting can be altered using `graph->dot-properties,` `vertex->dot,` and `edge->dot` as well as `edge-formatter,` `vertex-formatter,` `vertex-labeler,` and `edge-labeler`. These can be specified directly in the call to `graph->dot` or by creating subclasses of basic-graph, basic-vertex and basic-edge. 

The output can be a stream or pathname or one of the values `nil` or `t`. If output is `nil`, then graph->dot returns a string containing the DOT description. If it is `t`, then the DOT description is written to *standard-output*.

Here is an example;

    (let ((g (make-container 'graph-container :default-edge-type :directed)))
      (loop for (a b) in '((a b) (b c) (b d) (d e) (e f) (d f)) do
            (add-edge-between-vertexes g a b))
      (graph->dot g nil))

    \"digraph G {
    E []
    C []
    B []
    A []
    D []
    F []
    E->F []
    B->C []
    B->D []
    A->B []
    D->E []
    D->F []
    }\"

For more information about DOT file format, search the web for 'DOTTY' and 
'GRAPHVIZ'."))

;;; ---------------------------------------------------------------------------

(defgeneric graph->dot-properties (g stream)
  (:documentation "Unless a different graph-formatter is specified, this method is called by graph->dot to output graph-properties onto a stream. The function can assume that the openning and closing brackets will be taken care of by the graph->dot."))

;;; ---------------------------------------------------------------------------

(defgeneric vertex->dot (vertex stream)
  (:documentation "Unless a different vertex-formatter is specified with a keyword argument, this is used by graph->dot to output vertex formatting for `vertex` onto the `stream`. The function can assume that openning and closing square brackets and label have already been taken care of."))

;;; ---------------------------------------------------------------------------

(defgeneric edge->dot (edge stream)
  (:documentation "Used by graph->dot to output edge formatting for `edge` onto the `stream`. The function can assume that openning and closing square brackets and label have already been taken care of."))

;;; ---------------------------------------------------------------------------

(defgeneric generate-gnm (generator graph n m &key)
  (:documentation "Generate a 'classic' random graph G(n, m) with n vertexes and m edges."))

;;; ---------------------------------------------------------------------------

(defgeneric generate-gnp (generator graph n p &key)
  (:documentation  "Generate the Erd\"os-R\'enyi random graph G\(n, p\). I.e., a graph with n vertexes where
each possible edge appears with probability p. This implementation is from Efficient Generation
of Large Random Networks \(see batagelj-generation-2005 in doab\)."))

;;; ---------------------------------------------------------------------------

(defgeneric generate-undirected-graph-via-assortativity-matrix 
  (generator graph-class size edge-count kind-matrix assortativity-matrix
             vertex-labeler &key)
  (:documentation "This generates a random graph with 'size' vertexes. 
The vertexes can be in multiple different classes and the number of vertexes in each class is specified in the 'kind-matrix'. If the matrix has all fixnums, then it specifies the counts and these should add up to the size. If the kind-matrix contains non-fixnums then the values are treated as ratios.

The assortativity-matrix specifies the number of edges between vertexes of different kinds.

The vertex-labeler is a function of two parameters: the vertex kind and the index. It should return whatever the 'value' of the vertex ought to be."))

;;; ---------------------------------------------------------------------------

(defgeneric generate-undirected-graph-via-vertex-probabilities 
  (generator graph-class size kind-matrix probability-matrix vertex-labeler)
  (:documentation   "Generate an Erd\"os-R/'enyi like random graph having multiple vertex kinds. See the function
Gnp for the simple one vertex kind method.

Generator and graph-class specify the random number generator used and the class of the graph produced; Size
the number of vertexes. Kind matrix is a vector of ratios specifying the distribution of vertex kinds {0 ... \(1- k\)}.
The probability-matrix is a k x k matrix specifying the probability that two vertexes of the row-kind and the
column-kind will have an edge between them. vertex-labeler is a function of two arguments \(the kind and the vertex number\)
called to create values for vertexes. It will be called only once for each vertex created.

The clever sequential sampling technique in this implementation is from Efficient Generation
of Large Random Networks \(see batagelj-generation-2005 in moab\)."))

;;; ---------------------------------------------------------------------------

(defgeneric generate-scale-free-graph 
  (generator graph size kind-matrix add-edge-count
             other-vertex-kind-samplers vertex-labeler &key)
  (:documentation "Generates a 'scale-free' graph using preferential attachment -- too damn slow.

Size is the number of vertexes; 
kind-matrix is as in generate-undirected-graph-via-assortativity-matrix, etc.;
add-edge-count is the number of edges to add for each vertex;
other-vertex-kind-samplers are confusing...; and
vertex-labeler is used to create vertex elements \(as in other generators\)."))

;;; ---------------------------------------------------------------------------

(defgeneric generate-assortative-graph-with-degree-distributions
  (generator graph
             edge-count assortativity-matrix
             average-degrees
             degree-distributions
             vertex-labeler
             &key)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric generate-simple-preferential-attachment-graph (generator graph size minimum-degree)
  (:documentation "Generate a simple scale-free graph using the preferential attachment
mechanism of Barabasi and Albert. The implementation is from Efficient Generation
of Large Random Networks \(see batagelj-generation-2005 in moab\). Self-edges are possible."))

;;; ---------------------------------------------------------------------------

(defgeneric generate-preferential-attachment-graph 
  (generator graph size kind-matrix minimum-degree 
             assortativity-matrix 
             &key)
  (:documentation "Generate a Barabasi-Albert type scale free graph with multiple vertex kinds.

The idea behind this implementation is from Efficient Generation
of Large Random Networks \(see batagelj-generation-2005 in moab\)."))


;;; ---------------------------------------------------------------------------
;;; more
;;; ---------------------------------------------------------------------------

(defgeneric make-vertex-for-graph (graph &key &allow-other-keys)
  (:documentation "Creates a new vertex for graph `graph`. The keyword arguments include:

* vertex-class : specify the class of the vertex
* element      : specify the `element` of the vertex

and any other initialization arguments that make sense for the vertex class."))

;;; ---------------------------------------------------------------------------

(defgeneric tag-all-edges (thing)
  (:documentation "Sets the `tag` of all the edges of `thing` to true. [?? why does this exist?\]"))

;;; ---------------------------------------------------------------------------

(defgeneric untag-all-edges (thing)
  (:documentation "Sets the `tag` of all the edges of `thing` to nil.  [?? why does this exist?\]"))

;;; ---------------------------------------------------------------------------

(defgeneric untag-edges (edges)
  (:documentation "Sets the `tag` of all the edges of `thing` to true. [?? why does this exist?\]"))

;;; ---------------------------------------------------------------------------

(defgeneric tag-edges (edges)
  (:documentation "Sets the `tag` of all the edges of `thing` to true. [?? why does this exist?\]"))

;;; ---------------------------------------------------------------------------

(defgeneric replace-vertex (graph old new)
  (:documentation "Replace vertex `old` in graph `graph` with vertex `new`. The edge structure of the graph is maintained."))

;;; ---------------------------------------------------------------------------

(defgeneric add-edge-to-vertex (edge vertex)
  (:documentation "Attaches the edge `edge` to the vertex `vertex`."))

;;; ---------------------------------------------------------------------------

(defgeneric source-edges (vertex &optional filter)
  (:documentation "Returns a list of the source edges of `vertex`. [?? Could be a defun]."))

;;; ---------------------------------------------------------------------------

(defgeneric target-edges (vertex &optional filter)
  (:documentation "Returns a list of the target edges of `vertex`. [?? Could be a defun]."))

;;; ---------------------------------------------------------------------------

(defgeneric child-vertexes (vertex &optional filter)
  (:documentation "Returns a list of the vertexes to which `vertex` is connected by an edge and for which `vertex` is the source vertex. If the connecting edge is undirected, then the vertex is always counted as a source. [?? Could be a defun]."))

;;; ---------------------------------------------------------------------------

(defgeneric parent-vertexes (vertex &optional filter)
  (:documentation "Returns a list of the vertexes to which `vertex` is connected by an edge and for which `vertex` is the target vertex. If the connecting edge is undirected, then the vertex is always counted as a target. [?? Could be a defun]."))

;;; ---------------------------------------------------------------------------

(defgeneric neighbor-vertexes (vertex &optional filter)
  (:documentation "Returns a list of the vertexes to which `vertex` is connected by an edge disregarding edge direction. In a directed graph, neighbor-vertexes is the union of parent-vertexes and child-vertexes. [?? Could be a defun]."))

;;; ---------------------------------------------------------------------------

(defgeneric number-of-neighbors (vertex)
  (:documentation "Returns the number of neighbors of `vertex` (cf. `neighbor-vertexes`). [?? could be a defun]"))

;;; ---------------------------------------------------------------------------

(defgeneric in-cycle-p (graph start-vertex)
  (:documentation "Returns true if `start-vertex` is in some cycle in `graph`. This uses child-vertexes to generate the vertexes adjacent to a vertex."))

;;; ---------------------------------------------------------------------------

(defgeneric iterate-vertexes (thing fn)
  (:documentation "Calls `fn` on each of the vertexes of `thing`."))

;;; ---------------------------------------------------------------------------

(defgeneric edges (thing)
  (:documentation "Returns a list of the edges of `thing`."))

;;; ---------------------------------------------------------------------------

(defgeneric vertex-count (graph)
  (:documentation "Returns the number of vertexes in `graph`. [?? could be a defun]"))

;;; ---------------------------------------------------------------------------

(defgeneric vertexes (thing)
  (:documentation "Returns a list of the vertexes of `thing`."))

;;; ---------------------------------------------------------------------------

(defgeneric source-edge-count (vertex)
  (:documentation "Returns the number of source edges of vertex (cf. source-edges). [?? could be a defun]"))

;;; ---------------------------------------------------------------------------

(defgeneric target-edge-count (vertex)
  (:documentation "Returns the number of target edges of vertex (cf. target-edges). [?? could be a defun]"))

;;; ---------------------------------------------------------------------------

(defgeneric graph-roots (graph)
  (:documentation "Returns a list of the roots of graph. A root is defined as a vertex with no source edges \(i.e., all of the edges are out-going\). (cf. rootp) [?? could be a defun]"))

;;; ---------------------------------------------------------------------------

(defgeneric rootp (vertex)
  (:documentation "Returns true if `vertex` is a root vertex \(i.e., it has no incoming \(source\) edges\)."))

;;; ---------------------------------------------------------------------------

(defgeneric find-vertex-if (thing predicate &key key)
  (:documentation "Returns the first vertex in `thing` for which the `predicate` function returns non-nil. If the `key` is supplied, then it is applied to the vertex before the predicate is."))

;;; ---------------------------------------------------------------------------

(defgeneric find-edge-if (graph fn &key key)
  (:documentation "Returns the first edge in `thing` for which the `predicate` function returns non-nil. If the `key` is supplied, then it is applied to the edge before the predicate is."))

;;; ---------------------------------------------------------------------------

(defgeneric find-edges-if (thing predicate)
  (:documentation "Returns a list of edges in `thing` for which the `predicate` returns non-nil. [?? why no key function?]"))

;;; ---------------------------------------------------------------------------

(defgeneric find-vertexes-if (thing predicate)
  (:documentation "Returns a list of vertexes in `thing` for which the `predicate` returns non-nil. [?? why no key function?]"))

;;; ---------------------------------------------------------------------------

(defgeneric force-undirected (graph)
  (:documentation "Ensures that the graph is undirected (possibly by calling change-class on the edges)."))

;;; ---------------------------------------------------------------------------

(defgeneric traverse-elements (thing style fn)
  (:documentation "WIP"))

(defgeneric traverse-elements-helper (thing style marker fn)
  (:documentation "WIP"))

(defgeneric any-undirected-cycle-p (graph)
  (:documentation "Returns true if there are any undirected cycles in `graph`."))

(defgeneric edge-count (vertex)
  (:documentation "Returns the number of edges attached to `vertex`. Compare with the more flexible `vertex-degree`."))

(defgeneric topological-sort (graph)
  (:documentation "Returns a list of vertexes sorted by the depth from the roots of the graph. See also assign-level and graph-roots."))

(defgeneric assign-level (vertex level)
  (:documentation "Sets the depth of `vertex` to level and then recursively sets the depth of all of the children of `vertex` to \(1+ level\)."))

(defgeneric depth (graph)
  (:documentation "Returns the maximum depth of the vertexes in graph assuming that the roots are of depth 0 and that each edge distance from the roots increments the depth by one."))

(defgeneric make-vertex-edges-container (vertex container-class &rest args)
  (:documentation "Called during the initialization of a vertex to create the create the container used to store the edges incident to the vertex. The initarg :vertex-edges-container-class can be used to alter the default container class."))

(defgeneric other-vertex (edge value-or-vertex)
  (:documentation "Assuming that the value-or-vertex corresponds to one of the vertexes for `edge`, this method returns the other vertex of `edge`. If the value-or-vertex is not part of edge, then an error is signaled. [?? Should create a new condition for this]"))

(defgeneric find-edge-between-vertexes-if 
  (graph value-or-vertex-1 value-or-vertex-2 fn &key error-if-not-found?)
  (:documentation "Finds and returns an edge between value-or-vertex-1 and value-or-vertex-2 if one exists. Unless error-if-not-found? is nil, then a error will be signaled. [?? Error not signal, need test.]"))

(defgeneric vertices-share-edge-p (vertex-1 vertex-2)
  (:documentation "Return true if vertex-1 and vertex-2 are connected by an edge. [?? Compare adjacentp]"))

(defgeneric graph-edge-mixture-matrix (graph vertex-classifier &key edge-weight)
  (:documentation "Return the `mixing-matrix` of graph based on the classifier and the edge-weight function. The mixing matrix is a matrix whose runs and columns represent each class of vertex in the graph. The entries of the matrix show the total number of edges between vertexes of the two kinds. [?? Edge-weight is not used; also compare with graph-mixture-matrix.]"))

(defgeneric graph-mixing-matrix (graph vertex-classifier &key edge-weight)
  (:documentation "Return the `mixing-matrix` of graph based on the classifier and the edge-weight function. The mixing matrix is a matrix whose runs and columns represent each class of vertex in the graph. The entries of the matrix show the total number of edges between vertexes of the two kinds. [?? Edge-weight is not used; also compare with graph-edge-mixture-matrix.]"))

(defgeneric connected-components (graph)
  (:documentation "Returns a union-find-container representing the connected-components of `graph`."))

(defgeneric connected-component-count (graph)
  (:documentation "Returns the number of connected-components of graph."))

(defgeneric find-connected-components (graph)
  (:documentation "Returns a list of sub-graphs of `graph` where each sub-graph is a different connected component of graph. Compare with connected-components and connected-component-count."))

(defgeneric initialize-vertex-data (graph)
  (:documentation ""))

(defgeneric breadth-first-visitor (graph source fn)
  (:documentation ""))

(defgeneric breadth-first-search-graph (graph source)
  (:documentation ""))

(defgeneric mst-find-set (vertex)
  (:documentation ""))

(defgeneric mst-make-set (vertex)
  (:documentation ""))

(defgeneric mst-tree-union (v1 v2)
  (:documentation ""))

(defgeneric mst-link (v1 v2)
  (:documentation ""))

(defgeneric add-edges-to-graph (graph edges &key if-duplicate-do)
  (:documentation ""))

(defgeneric make-graph-from-vertexes (vertex-list)
  (:documentation "Create a new graph given a list of vertexes \(which are assumed to be from the same graph\). The new graph contains all of the vertexes in the list and all of the edges between any vertexes on the list."))

(defgeneric edge-lessp-by-weight (edge-1 edge-2)
  (:documentation "Returns true if the weight of edge-1 is strictly less than the weight of edge-2."))

(defgeneric minimum-spanning-tree (graph &key edge-sorter)
  (:documentation "Returns a minimum spanning tree of graph if one exists and nil otherwise."))

(defgeneric connected-graph-p (graph &key edge-sorter)
  (:documentation "Returns true if graph is a connected graph and nil otherwise."))

(defgeneric edge-lessp-by-direction (edge-1 edge-2)
  (:documentation "Returns true if and only if edge-1 is undirected and edge-2 is directed."))

(defgeneric out-edge-for-vertex-p (edge vertex)
  (:documentation "Returns true if the edge is connected to vertex and is either an undirected edge or a directed edge for which vertex is the source vertex of the edge."))

(defgeneric dfs (graph root fn &key out-edge-sorter)
  (:documentation ""))

(defgeneric dfs-visit (graph u fn sorter)
  (:documentation ""))

(defgeneric dfs-tree-edge-p (edge)
  (:documentation ""))

(defgeneric dfs-back-edge-p (edge)
  (:documentation ""))

(defgeneric dfs-forward-edge-p (edge)
  (:documentation ""))

(defgeneric dfs-cross-edge-p (edge)
  (:documentation ""))

(defgeneric dfs-edge-type (edge)
  (:documentation ""))

(defgeneric map-over-all-combinations-of-k-vertexes (graph k fn)
  (:documentation ""))

(defgeneric map-over-all-combinations-of-k-edges (vertex k fn)
  (:documentation ""))

(defgeneric complete-links (new-graph old-graph)
  (:documentation "Add edges between vertexes in the new-graph for which the matching  vertexes in the old-graph have edges. The vertex matching is done using `find-vertex`."))

(defgeneric subgraph-containing (graph vertex &optional depth)
  (:documentation "Returns a new graph that is a subset of `graph` that contains `vertex` and all of the other vertexes that can be reached from vertex by paths of less than or equal of length `depth`. If depth is not specified, then the entire sub-graph reachable from vertex will be returned. [?? Edge weights are always assumed to be one.]"))

;;; ---------------------------------------------------------------------------

(defgeneric weight (edge)
  (:documentation "Returns the weight of an edge. This defaults to 1.0 and can only be altered if the edge is a sub-class of `weighted-edge-mixin`."))
