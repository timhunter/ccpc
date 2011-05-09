
module Tree = struct
(*  Huet's zipper *)
  type 'a tree = Leaf of 'a | Branch of 'a * 'a tree list
  and 'a path = Top | Context of 'a tree list * 'a * 'a path * 'a tree list (* reversed list of left-siblings, parent label, path back to root, right sib list *)
  and 'a location = Loc of 'a tree * 'a path


  (* navigation *)
  let go_left (Loc(t,p)) = match p with 
      Top -> failwith "left of top" 
    | Context(l::left,label,up,right) -> Loc(l,Context(left,label,up,t::right))
    | Context([],_,_,_) -> failwith "left of first"

  let go_right (Loc(t,p)) = match p with 
      Top -> failwith "right of top" 
    | Context(left,label,up,r::right) -> Loc(r,Context(t::left,label,up,right))
    | _ -> failwith "right of last";; 

  let go_up (Loc(t,p)) = match p with  (* our Location's root-return-path becomes our path's root-return-path *)
      Top -> failwith "up of top" 
    | Context(left,label,up,right) -> Loc(Branch(label,(List.rev left)@(t::right)),up);;


  let go_down (Loc(t,p)) = match t with 
      Leaf _ -> failwith "down of item" 
    | Branch (label,(t1::trees)) -> Loc(t1,Context([],label,p,trees)) (* start at leftmost *)
    | _ -> failwith "down of empty";;

  (* leftmost child is the "1th" child *)
  let nth loc =
    let rec nthAux = function
	1 -> go_down loc
      | n ->
	  if n>0
	  then go_right (nthAux (n-1))
	  else failwith "nth expects a positive integer" in
      nthAux

  let rec gorn loc = function
      [] -> loc
    | addr::addrs -> gorn (nth loc addr) addrs

  let rec enclosing predicate ((Loc (t,p)) as l) =
    if predicate l
    then Some t
    else match p with
	Top -> None
      | _ -> enclosing predicate (go_up l)

  (* mutation *)
  let change (Loc(_,p)) t = Loc(t,p)

  let insert_right (Loc(t,p)) r = match p with 
      Top -> failwith "insert of top" 
    | Context(left,label,up,right) -> Loc(t,Context(left,label,up,r::right));; 

  let insert_left (Loc(t,p)) l = match p with 
      Top -> failwith "insert of top" 
    | Context(left,label,up,right) -> Loc(t,Context(l::left,label,up,right));; 

  let insert_down (Loc(t,p)) t1 = match t with 
      Leaf _ -> failwith "down of item" 
    | Branch (label,kids) -> Loc(t1,Context([],label,p,kids));; 

  let delete (Loc(_,p)) = match p with 
      Top -> failwith "delete of top" 
    | Context(left,label,up,r::right) -> Loc(r,Context(left,label,up,right))
    | Context(l::left,label,up,[]) -> Loc(l,Context(left,label,up,[]))
    | Context([],label,up,[]) -> Loc(Branch (label,[]),up);; 


  (* back and forth *)
  let rec tree_of_location = function
      Loc(t,Top) -> t
    | l -> tree_of_location (go_up l)

  let location_of_tree t = Loc (t,Top)

  let rec go_bottomLeft (Loc(t,p) as here) = match t with
      Leaf _ -> here
    | Branch (_,[]) -> here
    | Branch (label,(t1::trees)) -> go_bottomLeft (Loc(t1,Context([],label,p,trees)))

  let rec deepestNonPreceeding p (Loc(t,c) as here) =
    if p here
    then Some here
    else match c with
	Top -> None
      | Context(left,label,up,r::right) ->
	  let rsis = Loc(r,Context(t::left,label,up,right)) in
	    deepestNonPreceeding p (go_bottomLeft rsis)
      | Context(left,label,up,[]) -> deepestNonPreceeding p (go_up here)

  let nextUnfinished l =
    let unfinished (Loc(t,p)) = match t with
	Leaf _ -> false
      | Branch (label,[]) -> true
      | Branch (label,kids) -> false
    in
      deepestNonPreceeding unfinished (go_bottomLeft l)

  let concat = function
      "",y -> y
    | x,"" -> x
    | x,y -> x^" "^y

  let rec yield = function
      Leaf s -> s
    | Branch (_,kids) -> List.fold_left (fun x y -> concat (x,y)) "" (List.map yield kids)

  let labelOf = function
      Leaf name -> name
    | Branch (name,_) -> name

  let label_of_location = function
      Loc(t,p) -> labelOf t

  let count_daughters_of_location = function
      Loc ((Leaf _),_) ->  0
    | Loc ((Branch (_,kids)),_) -> List.length kids

  let is_leaf = function
      Loc ((Leaf x),_) -> true
    | Loc ((Branch _),_) -> false

  let is_rightmostchild = function
      Loc(t,p) ->
	begin match p with
	    Top -> true
	  | Context (_,_,_,[]) -> true
	  | _ -> false
	end


  let rec bracketOf = function
      Leaf s -> s
    | Branch (label,kids) -> "("^label^" "^(List.fold_left (fun x y -> concat (x,y)) "" (List.map bracketOf kids))^")"

  (* flavors of output *)
  let dot_of_tree_labeled t the_label = 
    let rec dtAux i = function
	Leaf label -> ("n"^(string_of_int i)^" [label = \""^label^"\"];" , i)
      | Branch (label,children) -> 
	  let parent = "n"^(string_of_int i)^"[label = \""^label^"\"];" in
	  let (subtrees,maximum) = List.fold_left
	    (fun (prevt,oldi) t ->
	       let (subtreei1,highest) = dtAux (oldi+1) t in
	       let branch = "n"^(string_of_int i)^"-> n"^(string_of_int (oldi+1))^";" in
		 (prevt^branch^subtreei1,highest)
	    )
	    ("",i)
	    children in
	    parent^subtrees,maximum
    in
      ("digraph "^the_label^" {\n node [shape = plaintext]; \n edge [arrowhead = none]; \n"^(Pervasives.fst (dtAux 0 t))^"}")
	
  let dot_of_tree t = dot_of_tree_labeled t "foobar"


  let qtree_of_tree t =
    let rec qtAux recursionLevel tree =
      if recursionLevel > 20 then failwith "your tree is too deep for qtree v3.1"
      else match tree with
	  Leaf label -> " "^label^" "
(*	| Branch (label,[]) ->  " $\\left[\\mbox{"^label^"}\\right]$ " *)
	| Branch (label,[]) ->  " \\Sought{"^label^"} " (* "expectation" *)
	| Branch (label,kids) ->
	    begin
	      if (List.length kids > 5)
	      then failwith "your tree is too wide for qtree v3.1"
	      else "[.{"^label^"}"^(List.fold_left (^) " " (List.map (qtAux (recursionLevel+1)) kids))^" ] "
	    end

    in
      qtAux 0 t

(*
write_file "heyYou.tex" (header (tuples_of_statelist (Utilities.range 12 30)));;
idea: pdfcrop the whole file
      then extract each page to a separate file using Acrobat
*)
  let write_file fname str =
    let oc = open_out fname in
      begin
	output_string oc str;
	close_out oc
      end


  (* input from bracketed S-expressions *)
  exception Noparse

  let explode s =
    let rec explode s i = if i< String.length s
    then (String.make 1 s.[i])::(explode s (i+1))
    else [] in
      (explode s 0)

  let implode l =  List.fold_right (^) l ""

  (* parser combinators a la Harrison *)

  (* his was || *)
  let ( |. ) = fun parser1 parser2 input ->
    try parser1 input
    with Noparse -> parser2 input

  (* his was ++ *)
  let ( &. ) = fun parser1 parser2 input ->
    let (result1,rest1) = parser1 input in
    let (result2,rest2) = parser2 rest1 in
      (result1,result2),rest2

  let rec many parser input =
    try
      let result,next = parser input in
      let results,rest = many parser next in
	(result::results),rest
    with Noparse -> [],input

  let some p = function
      [] -> raise Noparse
    | (h::t) -> if p h then (h,t) else raise Noparse


  let epsilon v rest = (v,rest)

  let opt a v =  a |. (epsilon v)

  let finished input = if input = [] then 0,input else raise Noparse

  let ( >> ) = fun parser treatment input ->
    let result,rest = parser input in
      (treatment result),rest

  (* lexer and parser for S-expressions -- bracketed strings *)

  type sexp_token = Lparen | Rparen | Name of string

  let sexp_lex =
    let collect (h,t) = h^(List.fold_right (^) t "") in
    let several p = many (some p) in
      
    let lp s = s = "(" in
    let rp s = s = ")" in
    let space s = s = " " || s = "\n" || s = "\t" in
    let nspace s = (not (space s)) && (not (lp s)) && (not (rp s)) in

    let lparen = (some (function s -> s ="(")) >> function _ -> Lparen in
    let rparen = (some (function s -> s =")")) >> function _ -> Rparen in

    let name = (some nspace) &. (several nspace) >> function x -> Name (collect x) in
    let token = (several space) &. (lparen |. rparen |. name) &. (several space) >> function ((_,z),_) -> z in
    let alltokens = ((many token) &. finished) >> Pervasives.fst in
      function x -> Pervasives.fst (alltokens (explode x))


  (* tree reader, fn from lexed S-expressions to trees *)

  (* this assumes the weird WSJ convention if an extra, unnamed TOP bracket *)
  let sexp_parse =
    let lp = function Lparen::rest -> ("(",rest)
      | _ -> raise Noparse in
    let rp = function Rparen::rest -> (")",rest)
      | _ -> raise Noparse in
    let nm = function (Name x)::rest -> (x,rest)
      | _ -> raise Noparse in

    let rec branch words = (lp &. nm &. (many ( (nm >> function x -> Leaf x) |. branch)) &. rp
			    >> function (((x,y),z),w) -> Branch (y,z))
      words in
    let outermost words = ((lp &. branch &. rp) >> function ((x,y),z) -> y ) words in 
      function x -> Pervasives.fst (outermost x)
	
  let rec sexp_of_tree = function
      Leaf s -> s
    | Branch (name,kids) -> "("^name^" "^(List.fold_left (fun x y -> concat (x,y)) "" (List.map sexp_of_tree kids))^")"


  let read_sexp bracket = sexp_parse (sexp_lex ("("^bracket^")"))



(*   let rec bracketOf = function *)
(*       Tree.Leaf s -> "(."^s^" ) " *)
(*     | Tree.Branch (label,kids) -> "(."^label^" "^(List.fold_left (fun x y -> Utilities.space x y) "" (List.map bracketOf kids))^") " *)
end
