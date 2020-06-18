
type derived_tree = Leaf of string | NonLeaf of (string option) * derived_tree * derived_tree

let rec latex_derived_tree t =
    match t with
    | Leaf(s)                   -> Printf.sprintf "[%s]" (if s = "" then "$\\epsilon$" else s)
    | NonLeaf(label,left,right) -> Printf.sprintf "[%s %s %s ]" (match label with None -> "" | Some s -> s)
                                                                (latex_derived_tree left)
                                                                (latex_derived_tree right)

