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

type 'a choice = 'a list * 'a * 'a list
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
        let one = Util.make_weight (Num.num_of_int 1) (Num.num_of_int 1) in
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


