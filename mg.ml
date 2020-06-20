open Util

type derived_tree = Leaf of string | NonLeaf of (string option) * derived_tree * derived_tree

(* This function combines two arguments of type [derived_tree option]. If either of the inputs 
 * is [None], then the result is [None] as well. Otherwise, the two trees get joined together. *)
let join_trees x y =
    match (x,y) with
    | (None,_) -> y
    | (_,None) -> x
    | (Some(t1), Some(t2)) -> Some(NonLeaf(None,t1,t2))  (* The inner None here is root label for the new tree *)

(* 
let (rules, start) = Grammar.get_input_grammar "./grammars/wmcfg/larsonian1.wmcfg" ;;
let no1 = List.hd (Derivation.get_n_best_from_grammar 1 rules start) ;;

let top10 = Derivation.get_n_best_from_grammar 10 rules start;;
let no1 = List.hd top10 ;;
let no6 = List.nth top10 5;;
let derived_no1 = Mg.derived_tree no1 ;; 
Mg.latex_derived_tree derived_no1 ;;
*)

(* type expansion = PublicTerminating of string | PublicNonTerminating of (string Nelist.t * tuplerecipe) *)
let derived_tree start_symbol tree =
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

let latex_derived_tree derived_tree =
    let rec print' dt =
        match dt with
        | Leaf(s) -> 
            Printf.sprintf "[%s]" (if s = "" then "$\\epsilon$" else s)
        | NonLeaf(label,left,right) -> 
            Printf.sprintf "[%s%s%s]" (match label with None -> "" | Some s -> s) (print' left) (print' right)   
    in 
    (* Printf.sprintf "\t\\begin{adjustbox}{max width = \\textwidth}\n" ^
    Printf.sprintf "\t\\begin{forest}\n" ^
    Printf.sprintf "\tfor tree={s sep=5mm, inner sep = 0, l-=3em}\n" ^
    Printf.sprintf "\t" ^ *)
    print' derived_tree (* ^ *)
    (* Printf.sprintf "\n" ^
    Printf.sprintf "\t\\end{forest}\n" ^
    Printf.sprintf "\t\\end{adjustbox}\n"  *)


(* MG-specific version of [Derivation.print_tuple].  
    The first three elements of [tuple_list] represents a triple: (left-of-head, head, right-of-head).
    Any following elements are "triples" that have been concatenated into a singleton. *)
let print_tuple_mg tree =
    let tuple = Derivation.get_tuple tree in
    (* split: 'a list -> int -> 'a list * 'a list 
        Splits a list into a pair of lists, where the first component is length [n:int] *)
    let split lst n =
        let rec split' fst snd i =
            match snd with
            | [] -> List.rev fst, []
            | x::xs -> if i = 0 then List.rev fst, x::xs else split' (x::fst) xs (i-1)
        in split' [] lst n
    in
    let (x, xs) = split tuple 3 in
    let others = 
        if List.length xs = 0
        then ""
        else "{,} \\texit{" ^ (String.concat "{,} " xs) ^ "}" in  
    (* Print [x] as triple surrounded by "(" and ")". Then print [xs] with LaTeX formatting, if any. *)
    Printf.sprintf "\\\\$\\langle$(\\textit{" ^ (String.concat "{,} " x) ^ "})" ^ others ^ "$\\rangle$"

let print_features table str = 
    match (Hashtbl.find_opt table str) with
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

let latex_derivation_tree dict start_symbol tree =
    let table = Grammar.get_guillaumin_dict dict in
    let print_terminal x y = 
        if (y = " ") || (y = "") 
        then Printf.sprintf "[%s\\\\$\\epsilon$]" x
        else Printf.sprintf "[%s\\\\\\textbf{%s}]" x y in
    let rec print' t =
        let node = Derivation.get_root_item t in
        let children = Derivation.get_children t in
        match (children, Rule.get_expansion (Derivation.get_rule t)) with
        | ([], Rule.PublicTerminating s) -> if (Hashtbl.mem table node) then (print_terminal node s) else ""
        | (_::_, Rule.PublicNonTerminating _) ->
            (* Only print start symbol, or nonterminals that have features in [table], if [table] exists *)
            if (node = start_symbol) || (Hashtbl.mem table node)
            then "[" ^ node ^ (print_tuple_mg t) ^ (print_features table node) ^ (String.concat " " (map_tr print' children)) ^ "]"
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

