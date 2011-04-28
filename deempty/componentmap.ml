open Rule

let numcomponentstbl = Hashtbl.create 101
let mappings = Hashtbl.create 101

let add_original_num (cat,_,sy) =
  Hashtbl.replace numcomponentstbl cat (List.length sy)

let initialize = List.iter add_original_num

let add_mapping cat nullcomplst =
  let originalnum = Hashtbl.find numcomponentstbl cat in
  let rec get_nonnullcomps i ret = 
    if i<originalnum
    then 
      if List.mem i nullcomplst
      then get_nonnullcomps (i+1) ret
      else get_nonnullcomps (i+1) (i::ret)
    else ret in
  let nonnullcomps = List.rev (get_nonnullcomps 0 []) in
  let (_,mapping) = List.fold_left (fun (i,acc) x -> ((i+1),((x,i)::acc))) (0,[]) nonnullcomps in
  Hashtbl.add mappings cat mapping

let compute_mappings nullcomponents_assoclst =
  List.iter (fun (cat,nullcomps) -> add_mapping cat nullcomps) nullcomponents_assoclst

let lookup cat comp =
  try
    List.assoc comp (Hashtbl.find mappings cat)
  with Not_found -> comp

let rewrite_rule (cat,children,sy) =
  (cat, children, List.map 
    (fun lst -> (List.map 
      (fun c -> match c with
        Component(i,j) -> Component(i,lookup (List.nth children i) j)
      | Epsilon -> Epsilon) lst)) sy)

let remap_components g = List.map rewrite_rule g
