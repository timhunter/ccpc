open Util

type derived_tree = Leaf of string | NonLeaf of (string option) * derived_tree * derived_tree

let derived_tree tree =
    (* argument to helper might not be a root tree, so it might return a non-singleton list *)
    let rec helper t =
        let children = Derivation.get_children t in
        match (children, Rule.get_expansion (Derivation.get_rule t)) with
        | ([],   Rule.PublicTerminating str) -> if str = " " then [Leaf("")] else [Leaf(str)]
        | (_::_, Rule.PublicNonTerminating (nts, recipe)) ->
            let subresults = map_tr helper children in
            (Rule.apply recipe subresults (fun x y -> NonLeaf(None,x,y)))
        | _ -> failwith "derived_tree: mismatch between tree structure and rules"
    in
    match (helper tree) with
    | [x] -> x
    | xs -> failwith (Printf.sprintf "derived_tree: expected a one-tuple but got an %d-tuple\n" (List.length xs))

let rec latex_derived_tree t =
    match t with
    | Leaf(s)                   -> Printf.sprintf "[%s]" (if s = "" then "$\\epsilon$" else s)
    | NonLeaf(label,left,right) -> Printf.sprintf "[%s %s %s ]" (match label with None -> "" | Some s -> s)
                                                                (latex_derived_tree left)
                                                                (latex_derived_tree right)

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
            then "[" ^ node ^ (Derivation.print_tuple t) ^ (print_features table node) ^ (String.concat " " (map_tr print' children)) ^ "]"
            else ""
        | _ -> failwith "Inconsistent tree in latex_tree"
    in
    print' tree

