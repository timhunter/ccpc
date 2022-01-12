open Util

type derived_tree = Leaf of string | NonLeaf of (string option) * derived_tree * derived_tree

type derived_tree2 = 
    | L of string 
    (* (head direction, tuplerecipe, left daughter, right daughter) *)
    | NL of string * ((int * int) list list) * derived_tree2 * derived_tree2 

(**********************************************************************************)
(* helper functions to create derived trees *)
let get_features table node =
    let separate_features str = Str.split (Str.regexp " ") str in
    let separate_subscript lst = [List.hd lst]::[List.tl lst] in
    match (Hashtbl.find_opt table (Rule.desituate node)) with
    | None -> [[]]
    | Some raw -> 
        let big_list = Str.split (Str.regexp ";:") raw in
        let temp = List.map (fun x -> separate_features x) big_list in
        List.append (separate_subscript (List.hd temp)) (List.tl temp)

(**********************************************************************************)

(* 
let (rules, start) = Grammar.get_input_grammar "./grammars/wmcfg/larsonian1.wmcfg" ;;
let top10 = Derivation.get_n_best_from_grammar 10 rules start;;
let no1b = List.hd top10;;
let table = Grammar.get_guillaumin_dict "./grammars/mcfgs/larsonian1.dict";; 
*)

let derived_tree2 dict tree =
    let table = Grammar.get_guillaumin_dict dict in
    (* Peel off the starting rule concatenation, if it exists. *)
    let starting_concatenation = [[(0, 0); (0, 1); (0, 2)]] in
    let tree' = 
        if (Rule.get_tuplerecipe (Derivation.get_rule tree) = starting_concatenation)
        then List.hd (Derivation.get_children tree)
        else tree
    in
    (* selector_is_linearly_left : (int * int) list list -> bool
        Given a tuple recipe for a Merge rule, returns true if the selector (which is always the left daughter of a 
        [Derivation.derivation_tree]) is printed linearly to the left of the selectee, and false if not.  *)
    let selector_is_linearly_left recipe =
        let head_recipe = List.nth recipe 1 in
        if (fst (List.hd head_recipe) = 0) then true else false
    in
    (* node_is_lexical: string Derivation.derivation_tree -> bool
        Given a derivation tree, returns true if the root item is lexical, and false if not.  *)
    let node_is_lexical t =
        let features = get_features table (Derivation.get_root_item t) in
        if (List.hd (List.hd features) = "::") then true else false
    in
    let rec helper t = 
        let root = Derivation.get_root_item t in
        let tuplerecipe = Rule.get_tuplerecipe (Derivation.get_rule t) in
        let children = Derivation.get_children t in
        match children with
        (* Terminal rule *)
        | [] -> L (String.concat "" (Derivation.get_tuple t))
        (* Move *)
        | x::[]  ->
            (* Get the index of the moving string from the tuple recipe of the current tree *)
            let moved_string_index = snd (List.hd (List.hd tuplerecipe)) in
            (* Get the moving string from the string tuple of the licensee tree. *)
            let moved_string = List.nth (Derivation.get_tuple x) moved_string_index in
            let licensor = L moved_string in
            let licensee = helper x in
            NL (">", tuplerecipe, licensor, licensee)
        (* Merge *)
        | x::y::[] ->
            let selector =
                if node_is_lexical x
                then L (String.concat "" (Derivation.get_tuple x))
                else helper x
            in
            let selectee =
                if node_is_lexical y
                then L (String.concat "" (Derivation.get_tuple y))
                else helper y
            in 
            if selector_is_linearly_left tuplerecipe
            then NL ("<", tuplerecipe, selector, selectee)
            else NL (">", tuplerecipe, selectee, selector)      
        | _ -> failwith "Mg.derived_tree2: mismatch between tree structure and rules"
    in helper tree'

(**********************************************************************************)

(* This function combines two arguments of type [derived_tree option]. If either of the inputs 
 * is [None], then the result is [None] as well. Otherwise, the two trees get joined together. *)
let join_trees (x:derived_tree option) (y:derived_tree option) =
    match (x,y) with
    | (None,_) -> y
    | (_,None) -> x
    | (Some(t1), Some(t2)) -> Some(NonLeaf(None,t1,t2))  (* The inner None here is root label for the new tree *)

let derived_tree tree =
    (* argument to helper might not be a root tree, so it might return a non-singleton list *)
    let rec helper t =
        let root_label = Derivation.get_root_item t in
        let children = Derivation.get_children t in
        let expansion = Rule.get_expansion (Derivation.get_rule t) in
        match (children, expansion) with
        | ([],   Rule.PublicTerminating str) ->
            (* If it's one of those dummy E nodes for head movement, then it contributes nothing to the derived tree. *)
            if root_label = "E" then
                [None]
            else
                [Some (Leaf(if str = " " then "" else str))]
        | (_::_, Rule.PublicNonTerminating (nts, recipe)) ->
            let subresults = map_tr helper children in
            (Rule.apply recipe subresults join_trees)
        | _ -> failwith "Mg.derived_tree: mismatch between tree structure and rules"
    in
    match (helper tree) with
    | [Some x] -> x
    | xs -> failwith (Printf.sprintf "Mg.derived_tree: expected a one-tuple but got an %d-tuple\n" (List.length xs))     


(**********************************************************************************)
(* helper functions to print LaTeX-formatted trees *)

(* MG-specific version of [Derivation.print_tuple].  
    The first three elements of [tuple_list] represents a triple: (left-of-head, head, right-of-head).
    Any following elements are "triples" that have been concatenated into a singleton. *)
let print_tuple_mg tree =
    let tuple_list = Derivation.get_tuple tree in
    (* add_epsilon : string list -> string list
        Adds LaTeX code to print empty string as epsilon symbol *)
    let rec add_epsilon lst =
        match lst with
        | [] -> []
        | x::xs -> if x = "" then "$\\epsilon$"::add_epsilon(xs) else x::add_epsilon(xs)
    in
    (* split: 'a list -> int -> 'a list * 'a list 
        Splits a list into a pair of lists, where the first component is length [n:int] *)
    let split lst n =
        let rec split' fst snd i =
            match snd with
            | [] -> List.rev fst, []
            | x::xs -> if i = 0 then List.rev fst, x::xs else split' (x::fst) xs (i-1)
        in split' [] lst n
    in
    let (x, xs) = split (add_epsilon tuple_list) 3 in
    let others = 
        if List.length xs = 0
        then ""
        else "{,} \\textit{" ^ (String.concat "{,} " xs) ^ "}" in  
    (* Print [x] as triple surrounded by "(" and ")". Then print [xs] with LaTeX formatting, if any. *)
    Printf.sprintf "\\\\$\\langle$(\\textit{" ^ (String.concat "{,} " x) ^ "})" ^ others ^ "$\\rangle$"

let print_features table str = 
    match (Hashtbl.find_opt table (Rule.desituate str)) with
    | None -> ""
    | Some raw ->
        (* Wrapper function to make regexp substitutions easier *)
        let replace rx subst string = Str.global_replace (Str.regexp rx) subst string in
        (* Replace certain strings with LaTeX code: tuple delimiter ";:" with "{,}", "=" with "{=}", and ">" with "$>$".
            Then, split string by " " *)
        let split = Str.split (Str.regexp " ") (replace ">" "$>$" (replace "=" "{=}" (replace ";:" "{,}" raw))) in
        (* "::" -> lexical (subscript 1) *)
        (* ":" -> non-lexical (subscript 0) *)
        let subscript = (if List.hd split = "::" then "_1" else "_0") in
        Printf.sprintf "::$\\langle$\\texttt{" ^ (String.concat " " (List.tl split)) ^ "}$\\rangle" ^ subscript ^"$"

let print_recipe rule =
    let tuplerecipe = Rule.get_tuplerecipe rule in
    let rec print' lst =
        match lst with
        | [] -> ""
        | (fst,snd)::[] -> Printf.sprintf "(%d{,}%d)" fst snd
        | (fst,snd)::xs -> Printf.sprintf "(%d{,}%d)" fst snd ^ ";" ^ print' xs
    in
    let tuplerecipe' = List.map (fun x -> print' x) tuplerecipe in
    Printf.sprintf "\\\\$\\langle$ " ^ String.concat " {,} " tuplerecipe' ^ " $\\rangle$"

(**********************************************************************************)

let latex_derivation_tree dict start_symbol tree =
    let table = Grammar.get_guillaumin_dict dict in
    let print_terminal x y = 
        if (y = " ") || (y = "") 
        then Printf.sprintf "[%s\\\\$\\epsilon$]" x
        else Printf.sprintf "[%s\\\\\\textbf{%s}]" x y in
    let rec print' t =
        let root = Rule.desituate (Derivation.get_root_item t) in
        let children = Derivation.get_children t in
        let rule = Derivation.get_rule t in
        match (children, Rule.get_expansion rule) with
        | ([], Rule.PublicTerminating s) -> if (Hashtbl.mem table root) then (print_terminal root s) else ""
        | (_::_, Rule.PublicNonTerminating _) ->
            (* Only print start symbol, or nonterminals that have features in [table]. *)
            if (root = (Rule.desituate start_symbol)) || (Hashtbl.mem table root)
            then "[" ^ root ^ (print_tuple_mg t) ^ (print_features table root) ^ (String.concat " " (map_tr print' children)) ^ "]"
            else ""
        | _ -> failwith "Mg.latex_derivation_tree: Inconsistent tree"
    in
    Printf.sprintf "\t\\begin{adjustbox}{max width = \\textwidth}\n" ^
    Printf.sprintf "\t\\begin{forest}\n" ^
    Printf.sprintf "\tfor tree={s sep=5mm, inner sep = 0, l-=3em}\n" ^
    Printf.sprintf "\t" ^
    print' tree ^
    Printf.sprintf "\n" ^
    Printf.sprintf "\t\\end{forest}\n" ^
    Printf.sprintf "\t\\end{adjustbox}\n" 

let latex_derived_tree2 derived_tree =
    let rec print' dt =
        match dt with
        | L str -> 
            Printf.sprintf "[%s]" (if str = "" then "$\\epsilon$" else str)
        | NL (direction, features, left, right) ->
            Printf.sprintf "[$%s$%s%s]" direction (print' left) (print' right)
    in 
    Printf.sprintf "\t\\begin{adjustbox}{max width = \\textwidth}\n" ^
    Printf.sprintf "\t\\begin{forest}\n" ^
    Printf.sprintf "\tfor tree={s sep=5mm, inner sep = 0, l-=3em}\n" ^
    Printf.sprintf "\t" ^
    print' derived_tree ^
    Printf.sprintf "\n" ^
    Printf.sprintf "\t\\end{forest}\n" ^
    Printf.sprintf "\t\\end{adjustbox}\n" 

