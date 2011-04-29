(** This module implements Huang's k-best hypergraph parsing algorithm.
*)

(** A vertex in the hypergraph is represented by a unique identifier. *)
type vertex = int
(** An edge consists of a single head and a list of tails, as well as a weight
  * function that calculates the weight of the head from the weights of the
  * tails. *)
type edge = {arity : int ;                              (** The number of tails of the hyperedge. *)
	     tails : vertex list ;                      (** The length of the tail list should be equal to arity. *)
	     head : vertex ;                            (** The head of the edge. *)
	     weight : Rational.rat list -> Rational.rat (** The weight function takes in the weights of the tails and returns the weight of the head.
							  * The function should throw an exception if the length of the input does not equal arity. *)
	    }

(** A module encapsulating a vertex.  We implement compare, hash, and equal so
  * that we can use this module as an element of a Set, a Map, and an ocamlgraph
    * Graph. *)
module Vertex = struct
  type t = vertex
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal v1 v2 = compare v1 v2 = 0
end

(** A module encapsulating an edge.  Two edges are equal if their arities,
  * heads, and tails are equal (since of course we cannot test the weight
  * functions for equality). *)
module Edge = struct
  type t = edge
  let compare e1 e2 =
    match Pervasives.compare e1.head e2.head with
      0 -> (match Pervasives.compare e1.arity e2.arity with
	      0 -> Pervasives.compare e1.tails e2.tails
	    | a -> a)
    | a -> a
end
module VertexSet = Set.Make(Vertex)
module VertexMap = Map.Make(Vertex)
module EdgeSet = Set.Make(Edge)

(** A hypergraph is a set of vertices and a set of edges. *)
type hypergraph = {vertices : VertexSet.t; edges : EdgeSet.t}

(** MyGraph will be the graph projection of the hypergraph, which we define in
  * order to detect cycles. *)
module MyGraph = Graph.Persistent.Digraph.ConcreteBidirectional(Vertex)
module MyComponents = Graph.Components.Make(MyGraph)

(* compare_derivations should be a parameter that can be set,
 * for example if we want to use a certain heuristic *)
let compare_derivations (e1, j1, w1) (e2, j2, w2) =
  if Edge.compare e1 e2 = 0 && j1 = j2 then 0
  else Rational.compare w1 w2

(** A derivation is an edge of arity n, a list [k1, ..., kn] where ki means to
  * use the ki'th best derivation from tail i of the edge, and a rational number
  * giving the weight of the derivation. *)
type derivation = edge * int list * Rational.rat
module Derivation = struct
  type t = derivation
  let compare = compare_derivations
end

(** We use DerivationHeap to find the best derivation (according to
    * compare_derivations) from a set. *)
module DerivationHeap = Heap.Functional(Derivation)

(** Vector addition.  Adds two equal-length lists of integers, componentwise.
  * @raise Invalid_argument if the two lists have different lengths. *)
let ( >+ ) = List.map2 ( + )

(** v >. i returns the ith component of vector (list) v.
  * @raise Failure "nth" if i is greater than or equal to the length of v.
  * @raise Invalid_argument "List.nth" if i is negative. *)
let ( >. ) v i = List.nth v i

(** @return The edges in a hypergraph that have a certain vertex as their head.
  * @param h The hypergraph in which to look for edges.
  * @param v The vertex for which to find the edges with v as their head. *)
let backward_star h v =
  EdgeSet.filter (fun e -> e.head = v) h.edges

(** Indicates whether we have added any candidate derivations for a vertex. *)
let is_candidates_empty cands v =
  not (VertexMap.mem v cands) || (VertexMap.find v cands = DerivationHeap.empty)

(** Add a candidate derivation to the heap for a vertex.
  * @return The resulting candidate heaps *)
let add_to_candidates v d cands =
  let heap = if VertexMap.mem v cands then VertexMap.find v cands
             else DerivationHeap.empty in
  VertexMap.add v (DerivationHeap.add d heap) cands

(** Remove the best candidate from the heap for a given vertex.
  * @return The resulting candidate heaps *)
let remove_from_candidates v cands =
  if VertexMap.mem v cands then
    let heap = VertexMap.find v cands in
    if heap = DerivationHeap.empty then
      cands
    else
      VertexMap.add v (DerivationHeap.remove heap) cands
  else
    cands

(** Indicates whether a certain derivation is in the candidates for a vertex. *)
let is_in_candidates v (e, j) cands =
  (VertexMap.mem v cands) &&
  (DerivationHeap.fold (fun (e', j', _) b ->
       b || (Edge.compare e e' = 0 && j = j'))
     (VertexMap.find v cands) false)

(** @return The number of derivations we have calculated for a given vertex. *)
let number_of_derivations derivs v =
  if VertexMap.mem v derivs then
    List.length (VertexMap.find v derivs)
  else 0

(** @return The nth best derivation we have calculated for a given vertex. *)
let get_nth_derivation derivs v n =
  if VertexMap.mem v derivs then
    try List.nth (VertexMap.find v derivs) n with Failure "nth" -> raise Not_found
  else
    raise Not_found

(** @return The worst derivation we have calculated for a given vertex. *)
let get_last_derivation derivs v =
  get_nth_derivation derivs v ((number_of_derivations derivs v) - 1)

(** @return The best derivation we have calculated for a given vertex. *)
let get_first_derivation derivs v =
  get_nth_derivation derivs v 0

(** Add a derivation to the list of derivations for a vertex.
  * @return The resulting lists of derivations. *)
let add_to_derivations v d derivs =
  let ds = if VertexMap.mem v derivs then VertexMap.find v derivs else [] in
  VertexMap.add v (ds @ [d]) derivs

(** Using already calculated nth best derivation weights for the tails of an
  * edge, calculate the whole derivation weight.
  * @param e The edge for which to calculate the weight.
  * @param j The list of which derivation to use for each tail.
  * @param derivs Stores already calculated derivations for vertices. *)
let calculate_derivation_weight (e, j) derivs =
  let tail_weights = List.fold_right2 (fun tail ji weights -> 
      let (_, _, w) = get_nth_derivation derivs tail ji in
      w::weights)
    e.tails j []
  in
  e.weight tail_weights

(** Initializes the candidate derivations for a vertex by taking every edge in
  * the backward star of that vertex and using the best derivation for each tail
  * in that edge.  Only take the best k of these edges (if there are more than
  * k).
  * @param hypergraph The hypergraph we are parsing.
  * @param best_derivs Stores the single best derivation (i.e., a selection of a
  *   single edge) for each vertex.
  * @param cands The current candidates for each vertex.
  * @param v The vertex for which to initialize its candidates.
  * @param k How many derivations we want.
  * @return The resulting candidates. *)
let get_candidates hypergraph best_derivs cands v k =
  (* Returns an all-zero vector of the specified length. *)
  let rec zero_vector arity = 
    if arity = 0 then [] else 0::(zero_vector (arity - 1))
  in
  (* Add the first k elements (or up to the length of the list) of a list of
   * derivations to the candidates for vertex v. *)
  let rec add_k_candidates cands deriv_list k =
    if k <= 0 then cands
    else
      match deriv_list with
	[] -> cands
      | d::ds -> add_k_candidates (add_to_candidates v d cands) ds (k - 1)
  in
  let bs = backward_star hypergraph v in
  (* The list of best derivations for each edge going into v. *)
  let temp = EdgeSet.fold (fun e l ->
      let j = zero_vector e.arity in
      let w = calculate_derivation_weight (e, j) best_derivs in
      (e, j, w)::l)
    bs [] in
  let temp_sorted = List.rev (List.sort Derivation.compare temp) in
  add_k_candidates cands temp_sorted k

(** Detect cycles in the hypergraph and set the weights of all edges involved
  * in a cycle to zero. *)
let set_cycle_weights_to_zero hypergraph =
  (* Get the graph projection of the hypergraph by including an edge (u, v) if
   * u is one of the tails of an edge with v as its head in the hypergraph. *)
  let graph = VertexSet.fold (fun v g -> MyGraph.add_vertex g v)
                             hypergraph.vertices MyGraph.empty in
  let graph = EdgeSet.fold (fun e g -> List.fold_left
			      (fun g v -> MyGraph.add_edge g v e.head)
			      g e.tails)
                           hypergraph.edges graph in
  (* Find strongly connected components in the graph projection. *)
  let (_, component) = MyComponents.scc graph in
  let in_same_component v1 v2 = component v1 = component v2 in
  let new_hypergraph = {vertices = hypergraph.vertices;
			edges = EdgeSet.empty} in
  (* If there is a tail of an edge in the same component as the head, that means
   * that the edge is part of a cycle.  Set its weight function to be constantly
   * zero. *)
  EdgeSet.fold (fun e h ->
      if List.exists (fun v -> in_same_component v e.head) e.tails then
        let e' = {arity = e.arity; tails = e.tails; head = e.head;
		  weight = fun l -> Rational.zero} in
        {vertices = h.vertices; edges = EdgeSet.add e' h.edges}
      else 
        {vertices = h.vertices; edges = EdgeSet.add e h.edges})
    hypergraph.edges new_hypergraph
  
(** Calculate the k best derivations of a target vertex in a hypergraph.
  * @param hypergraph The hypergraph we are parsing.
  * @param best_derivs The pre-computed single best derivations for each vertex
  *   in the hypergraph.
  * @param target_vertex The vertex for which we want the k best derivations.
  * @param global_k The number of derivations we want.
  * @return A pair (derivs, cands).  derivs is a map from vertices to lists of
  *   derivations, in decreasing order of weight (so the first element of the
  *   list is the best derivation).  cands is a map from vertices to heaps of
  *   candidate derivations that have not been added to derivs.  This will
  *   probably be of no use to the user. *)
let k_best hypergraph best_derivs target_vertex global_k =
  (* Detect cycles and set their weights to zero. *)
  let hypergraph = set_cycle_weights_to_zero hypergraph in
  (* Find the k best derivations for a certain vertex v. *)
  let rec lazy_kth_best derivs cands v k =
(*    let _ = print_endline ("lazy_kth_best " ^ (string_of_int v) ^ " " ^ (string_of_int k)) in let _ = ignore (read_line ()) in*)
    (* This function is called until we have calculated k derivations for vertex
     * v, or until we have run out of candidates. *)
    let rec while_less_than_k derivs cands =
      let num_derivs = number_of_derivations derivs v in
      if num_derivs <= k then
	let (derivs, cands) = 
	  if num_derivs > 0 then
	    (* Take the last derivation for v and expand it, adding new
	     * candidates for v. *)
	    let (e, j, _) = get_last_derivation derivs v in
	    lazy_next derivs cands e j
	  else (derivs, cands) in
	if not (is_candidates_empty cands v) then
	  (* Take the best candidate and add it to the list of derivations. *)
	  let cands_v = VertexMap.find v cands in
	  let d = DerivationHeap.maximum cands_v in
	  let derivs = add_to_derivations v d derivs in
	  let cands = remove_from_candidates v cands in
	  while_less_than_k derivs cands
	else (derivs, cands)
      else (derivs, cands)
    in
    (* Initialize v's candidates if necessary. *)
    let cands = if VertexMap.mem v cands then cands
                else get_candidates hypergraph best_derivs cands v global_k in
    while_less_than_k derivs cands
  (* Expand a derivation by adding its "neighbors" to the candidates for v. *)
  and lazy_next derivs cands e j =
(*    let _ = print_endline ("lazy_next (" ^ (List.fold_left (fun s t -> s ^ (string_of_int t) ^ " ") "" e.tails) ^ "-> " ^ (string_of_int e.head) ^ ") (" ^ (List.fold_left (fun s ji -> s ^ (string_of_int ji) ^ ", ") "" j) ^ ")") in let _ = ignore (read_line ()) in*)
    (* Return a vector of length arity that is all 0s except with a 1 at index
     * i. *)
    let get_b_vector arity i =
      let rec get_b_vector_rec j =
	if j = arity then []
	else
	  (if j = i then 1 else 0)::(get_b_vector_rec (j + 1))
      in
      if i >= arity then raise (Invalid_argument "get_b_vector")
      else get_b_vector_rec 0
    in
    (* For the ith tail of edge e, get the next worst derivation than is
     * specified by the ith index of the vector j. *)
    let rec iterate_through_tails derivs cands i tails =
      match tails with
	[] -> (derivs, cands)
      | tail_i::rest -> (
	  let bi = get_b_vector e.arity i in
	  let j' = j >+ bi in
	  let j'_i = j' >. i in
	  let (derivs, cands) = lazy_kth_best derivs cands tail_i j'_i in
	  let cands =
	    (* If we succeeded in calculating the next derivation and if it's
	     * not already in the candidates, add it *)
	    if j'_i < number_of_derivations derivs tail_i &&
	       not (is_in_candidates e.head (e, j') cands) then
	      let weight = calculate_derivation_weight (e, j') derivs in
	      add_to_candidates e.head (e, j', weight) cands
	    else cands in
	  iterate_through_tails derivs cands (i + 1) rest)
    in
    iterate_through_tails derivs cands 0 e.tails
  in
  (* Start with empty derivs and cands.  Subtract 1 from global_k because we
   * index the derivations from 0 internally, so getting the 3 best derivations
   * means getting the 0th, 1st, and 2nd best. *)
  lazy_kth_best VertexMap.empty VertexMap.empty target_vertex (global_k - 1)

let weight_fun p tails = p *. (List.fold_left ( *. ) 1. tails)

(*
let vp161 = {arity = 2; tails = [2;3]; head = 1; weight = weight_fun 0.4}
let vp162 = {arity = 3; tails = [2;4;5]; head = 1; weight = weight_fun 0.6}
let vbd12 = {arity = 0; tails = []; head = 2; weight = weight_fun 1.}
let np26 = {arity = 2; tails = [4;5]; head = 3; weight = weight_fun 0.8}
let np23 = {arity = 0; tails = []; head = 4; weight = weight_fun 1.}
let pp36 = {arity = 0; tails = []; head = 5; weight = weight_fun 1.}

let edge_list = [vp161; vp162; vbd12; np26; np23; pp36]

let edges = List.fold_left (fun es e -> EdgeSet.add e es) EdgeSet.empty edge_list
let vertices = List.fold_left (fun vs v -> VertexSet.add v vs) VertexSet.empty [1;2;3;4;5]
let hypergraph = {edges = edges; vertices = vertices}

let best_deriv_list = [(1, (vp162, [0;0;0], 0.6));
		       (2, (vbd12, [], 1.));
		       (3, (np26, [0;0], 0.8));
		       (4, (np23, [], 1.));
		       (5, (pp36, [], 1.))]

let best_derivs = List.fold_left (fun bds (v, d) -> add_to_derivations v d bds) VertexMap.empty best_deriv_list

let (derivs, cands) = k_best hypergraph best_derivs 1 2
  *)

let weight = List.fold_left Rational.plusr Rational.zero

let s041 = {arity = 2; tails = [3;4]; head = 1; weight = weight}
let s042 = {arity = 2; tails = [2;10]; head = 1; weight = weight}
let a041 = {arity = 2; tails = [3;4]; head = 2; weight = weight}
let a021 = {arity = 2; tails = [5;6]; head = 3; weight = weight}
let b241 = {arity = 2; tails = [8;9]; head = 4; weight = weight}
let b242 = {arity = 2; tails = [7;10]; head = 4; weight = weight}
let a241 = {arity = 2; tails = [8;9]; head = 7; weight = weight}
let c011 = {arity = 0; tails = []; head = 5; weight = fun l -> (1, 2)}
let d121 = {arity = 0; tails = []; head = 6; weight = fun l -> (1, 2)}
let b441 = {arity = 2; tails = [12;13]; head = 10; weight = weight}
let b442 = {arity = 2; tails = [10;11]; head = 10; weight = weight}
let a441 = {arity = 2; tails = [12;13]; head = 11; weight = weight}
let a442 = {arity = 2; tails = [10;11]; head = 11; weight = weight}
let c441 = {arity = 0; tails = []; head = 12; weight = fun l -> Rational.zero}
let d441 = {arity = 0; tails = []; head = 13; weight = fun l -> Rational.zero}
let c231 = {arity = 0; tails = []; head = 8; weight = fun l -> (1, 2)}
let d341 = {arity = 0; tails = []; head = 9; weight = fun l -> (1, 2)}

let edge_list = [s041; s042; a041; a021; b241; b242; c011; d121; a241; c231; d341; b441; b442; a441; a442; c441; d441]

let edges = List.fold_left (fun es e -> EdgeSet.add e es) EdgeSet.empty edge_list
let vertices = List.fold_left (fun vs v -> VertexSet.add v vs) VertexSet.empty [1;2;3;4;5;6;7;8;9;10;11;12;13]

let hypergraph = {edges = edges; vertices = vertices}

let best_deriv_list = [(1, (s041, [0;0], (2, 1)));
		       (2, (a041, [0;0], (2, 1)));
		       (3, (a021, [0;0], (1, 1)));
		       (4, (b241, [0;0], (1, 1)));
		       (5, (c011, [], (1, 2)));
		       (6, (d121, [], (1, 2)));
		       (7, (a241, [0;0], (1, 1)));
		       (8, (c231, [], (1, 2)));
		       (9, (d341, [], (1, 2)));
		       (10, (b441, [0;0], (0, 1)));
		       (11, (a441, [0;0], (0, 1)));
		       (12, (c441, [], (0, 1)));
		       (13, (d441, [], (0, 1)))]

let best_derivations = List.fold_left (fun bds (v, d) -> add_to_derivations v d bds) VertexMap.empty best_deriv_list

let (derivs, cands) = k_best hypergraph best_derivations 1 2
