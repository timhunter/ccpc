module MyList : sig

        type ('a,'b) t   (* a non-empty list of 'a, with a 'b labelling each link *)

        val singleton : 'a -> ('a,'b) t
        val hd : ('a,'b) t -> 'a
        val unpack : ('a,'b) t -> 'a * ('b * ('a,'b) t) option
        val extend : ('a,'b) t -> 'b -> 'a -> ('a,'b) t
        val contents : ('a,'b) t -> 'a list
        val labels : ('a,'b) t -> 'b list
        val append : ('a,'b) t -> 'b -> ('a,'b) t -> ('a,'b) t
        val split : ('a,'b) t -> 'a -> (('a,'b) t * 'b * ('a,'b) t)
        val rotate : ('a,'b) t -> 'a -> ('a,'b) t

end = struct

        type ('a,'b) t = End of 'a | NonEnd of ('a * 'b * ('a,'b) t)

        let singleton x = End(x)
        let hd     = function End(x) -> x        | NonEnd(x,_,_) -> x
        let unpack = function End(x) -> (x,None) | NonEnd(x,y,rest) -> (x,Some(y,rest))

        let rec extend lst b a =
                match lst with
                | End(x) -> NonEnd(x,b,End(a))
                | NonEnd(x,y,rest) -> NonEnd(x, y, extend rest b a)

        let rec contents lst =
                match lst with
                | End(x) -> [x]
                | NonEnd(x,_,rest) -> x::(contents rest)

        let rec labels lst =
                match lst with
                | End(_) -> []
                | NonEnd(_,y,rest) -> y::(labels rest)

        let rec append lst1 lbl lst2 =
                match lst1 with
                | End(x) -> NonEnd(x,lbl,lst2)
                | NonEnd(x,y,rest) -> NonEnd(x, y, (append rest lbl lst2))

        (* Splits the list at the first link which leads to element e. *)
        (* The third component of the result should begin with element e. *)
        (* Result type is:   ('a,'b) t * 'b * ('a,'b) t      *)
        let split lst e =
                let rec helper acc link rest =
                        match rest with
                        | End(x) ->            if x = e then (acc,link,rest) else raise Not_found
                        | NonEnd(x,y,rest') -> if x = e then (acc,link,rest) else (helper (extend acc link x) y rest')
                in
                match lst with
                | End(x) -> failwith "split: Can't split a one-element list!"
                | NonEnd(x,y,rest) -> helper (singleton x) y rest

        (* Assuming lst starts and ends with the same element, rotate it so that 
         * it starts and ends with e. *)
        let rotate lst e =
                (* First element in before must be the same as the last element in after *)
                let (before,link,after) = split lst e in
                let rec overwrite_end lst1 lst2 =   (* appends the two, by clobbering the end of lst1 *)
                        match lst1 with
                        | End(x) -> lst2
                        | NonEnd(x,y,rest) -> NonEnd(x, y, overwrite_end rest lst2)
                in
                extend (overwrite_end after before) link e

end

(***********************************************************************************)

type 'a history = ('a, 'a list * 'a list * Util.weight) MyList.t

let singleton x = MyList.singleton x

let root lst = MyList.hd lst

let last lst =
        let rec helper (ls,rs) l =
                match (MyList.unpack l) with
                | (x, None) -> (ls,x,rs)
                | (_, Some((ls',rs',_),rest)) -> helper (ls',rs') rest
        in helper ([],[]) lst

let extend lst wt (ls,x,rs) = MyList.extend lst (ls,rs,wt) x

let to_list lst = MyList.contents lst

let weight_product lst =
        let weights = Util.map_tr (fun (_,_,x) -> x) (MyList.labels lst) in
        let one = Util.make_weight 1 1 in
        List.fold_left Util.mult_weights one weights

let latex_history_tikz f hist =
        let rec worker h =   (* worker should produce a string of the form "node {...} ..." *)
                let make_node s = Printf.sprintf "node %s" s in
                let make_child s = Printf.sprintf "child {%s}" s in
                let make_child_list xs = String.concat " " (Util.map_tr make_child (Util.map_tr make_node (Util.map_tr f xs))) in
                match (MyList.unpack h) with
                | (x, None)                  -> Printf.sprintf "node %s" (f x)
                | (x, Some((ls,rs,wt),rest)) -> Printf.sprintf "node %s %s %s %s" (f x) (make_child_list ls) (make_child (worker rest)) (make_child_list rs)
                (* "node {foo} child {node {bar}} child {node {baz}}" *)
        in
        Printf.sprintf "\\tikz \\path %s ;" (worker hist)

let show_history f h =
        Printf.sprintf "%s %s"
        (Util.show_weight_float (weight_product h))
        (String.concat " -- " (Util.map_tr f (to_list h)))

let show_history_full f h =
        let rec worker hist =
                match (MyList.unpack hist) with
                | (a, None)       -> f a
                | (a, Some ((ls,rs,wt),rest)) -> Printf.sprintf "%s -- (%s,%s,%s) -- %s"
                                                                (f a) (Util.show_list f ls) (Util.show_list f rs) (Util.show_weight_float wt) (worker rest)
        in
        Printf.sprintf "%s %s" (Util.show_weight_float (weight_product h)) (worker h)

let rotate x hist = MyList.rotate hist x

let canonicalise cmp hist =
        let start = List.hd (List.sort cmp (MyList.contents hist)) in
        rotate start hist

let try_ending_cycle lst =
        match (MyList.unpack lst) with
        | (_, None) -> None   (* lst is too short to have any cycles *)
        | _ ->
                let (_,endpoint,_) = last lst in
                let (before, link, after) = MyList.split lst endpoint in
                (* after begins with the first occurrence of endpoint in the list *)
                match (MyList.unpack after) with
                | (_, None) -> None                                          (* after is just the last link in the list, hence no cycle *)
                | _ -> Some (MyList.extend before link endpoint, after)      (* after starts somewhere else, in the middle of the list, hence it's a cycle *)

let rec choices lst =
        match lst with
        | [] -> []
        | (x::xs) -> let rest = choices xs in
                     let add_x (ls,y,rs) = (x::ls,y,rs) in
                     ([],x,xs) :: (List.map add_x rest)

let get_cycles tree =
        let rec worker history t =
                let (_,endpoint,_) = last history in
                assert (endpoint = Derivation.get_root_item t) ;
                let (new_history, cycles_here) =
                        match (try_ending_cycle history) with
                        | None -> (history, [])
                        | Some (before,cycle) -> (before, [(before,cycle)])
                in
                let f (ls,child,rs) = worker (MyList.extend new_history (Util.map_tr Derivation.get_root_item ls, 
                                                                         Util.map_tr Derivation.get_root_item rs, 
                                                                         Rule.get_weight (Derivation.get_rule t)) (Derivation.get_root_item child)) child in
                let cycles_from_children = Util.map_tr f (choices (Derivation.get_children t)) in
                cycles_here @ (List.concat cycles_from_children)
        in
        worker (singleton (Derivation.get_root_item tree)) tree

(* Returns the path from the root of tree extending downward as 
 * far as possible using nodes that satisfy the predicate p. 
 * Fails if multiple children at a particular level satisfy p.*)
let get_path p tree =
        let rec worker history t =
                let (_,endpoint,_) = last history in
                let root = Derivation.get_root_item t in
                assert (endpoint = root) ;
                assert (p root) ;
                let child_choices = choices (Derivation.get_children t) in
                let candidate_choices = List.filter (fun (_,x,_) -> p (Derivation.get_root_item x)) child_choices in
                match candidate_choices with
                | [] -> history
                | (ls,child,rs)::[] -> let extended_history = MyList.extend history (Util.map_tr Derivation.get_root_item ls, 
                                                                                     Util.map_tr Derivation.get_root_item rs, 
                                                                                     Rule.get_weight (Derivation.get_rule t)) (Derivation.get_root_item child) in
                                       worker extended_history child
                | _ -> failwith (Printf.sprintf "get_path: not sure how to choose among these candidates: %s" 
                                                (Util.show_list (fun (_,x,_) -> Derivation.get_root_item x) candidate_choices))
        in
        worker (singleton (Derivation.get_root_item tree)) tree

