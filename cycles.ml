
(***********************************************************************************)

let latex_start = ["\\documentclass{article}"; 
                   "\\usepackage{fullpage}";
                   "\\usepackage{tikz}";
                   "\\tikzset{every tree node/.style={align=center,anchor=north}} % allow newlines inside nodes of qtree-style trees";
                   "\\tikzset{every node/.style={align=center,anchor=north}}      % and other nodes too";
                   "\\tikzset{level distance=30pt, sibling distance=100pt}";
                   "\\tikzset{every picture/.style={baseline=-12pt}} % put the baseline at the top of every tikz picture";
                   "\\begin{document}"]

let latex_end = ["\\end{document}"]

(***********************************************************************************)

(* returns: string list list *)
let get_child_lists rules nt =
        let relevant_rules = List.filter (fun r -> Rule.get_nonterm r = nt) rules in
        let get_children r =
                match Rule.get_expansion r with
                | Rule.PublicTerminating _ -> []
                | Rule.PublicNonTerminating (nelist,_) -> Nelist.to_list nelist
        in
        Util.map_tr get_children relevant_rules

(* returns: (weight * string list) list *)
let get_child_lists' rules nt =
        let relevant_rules = List.filter (fun r -> Rule.get_nonterm r = nt) rules in
        let get_children r =
                match Rule.get_expansion r with
                | Rule.PublicTerminating _ -> (Rule.get_weight r, [])
                | Rule.PublicNonTerminating (nelist,_) -> (Rule.get_weight r, Nelist.to_list nelist)
        in
        Util.map_tr get_children relevant_rules

let rec choices lst =
        match lst with
        | [] -> []
        | (x::xs) -> let rest = choices xs in
                     let add_x (ls,y,rs) = (x::ls,y,rs) in
                     ([],x,xs) :: (List.map add_x rest)

(* The symbol to start searching from is the one at the end of the history h *)
let rec find_cycles_inner rules h =
        let (_,nt,_) = Path.last h in
        match (Path.try_ending_cycle h) with
        | Some (before,cycle) -> [(before,cycle)]
        | None ->
                let process_child_list children wt =
                        let choices_with_weights = Util.map_tr (fun choice -> (wt,choice)) (choices children) in
                        List.concat (Util.map_tr (fun (wt,choice) -> find_cycles_inner rules (Path.extend h wt choice)) choices_with_weights)
                in
                List.concat (Util.map_tr (fun (wt,children) -> process_child_list children wt) (get_child_lists' rules nt))

let find_cycles rules start_symbol =
        Util.map_tr (fun (b,c) -> (Path.canonicalise compare c, b)) (find_cycles_inner rules (Path.singleton start_symbol))

let verbatim s =
        let escape_underscores x = Str.global_replace (Str.regexp "_") "\\_" x in
        Printf.sprintf "%s" (escape_underscores s)

let latex_tree_node dict nt =
        match dict with
        | None -> Printf.sprintf "{%s}" (verbatim nt)
        | Some tbl -> try let features = Hashtbl.find tbl (Grammar.desituate nt) in
                          let features_cleaned = Str.global_replace (Str.regexp ": ") ":~" features in
                          let components = Str.split (Str.regexp ";") features_cleaned in
                          let combined = "(" ^ (String.concat ", " components) ^ ")" in
                          Printf.sprintf "{%s\\\\[-0.2\\baselineskip]\\texttt{%s}}" (verbatim nt) (verbatim combined)
                      with Not_found -> Printf.sprintf "{%s}" (verbatim nt)

let compare_by_weight h1 h2 =
        let w1, w2 = Path.weight_product h1, Path.weight_product h2 in
        Util.compare_weights w1 w2

(* returns:  ('a * 'b list) list *)
let group assocs : (('a * 'b) list) =
        let tbl = Hashtbl.create 10 in
        List.iter (fun (a,b) -> Hashtbl.add tbl a b) assocs ;
        let keys = Hashtbl.fold (fun k v l -> if List.mem k l then l else k::l) tbl [] in
        let result = Util.map_tr (fun k -> (k, Hashtbl.find_all tbl k)) keys in
        result

let latex_cycle dict (cycle, ways_to_cycle) =

        let caption = "Cost of cycle: " ^ (Util.show_weight_float (Path.weight_product cycle)) in
        let start = ["\\begin{figure}"; "\\footnotesize"; Printf.sprintf "\\caption{%s}" caption] in

        let tikz_diagram = Path.latex_history_tikz (latex_tree_node dict) cycle in
        (* let summarise w = Printf.sprintf "%s %s --> ... --> %s" (Util.show_weight_float (Path.weight_product w)) (Path.root w) (let (_,x,_) = Path.last w in x) in *)
        let summarise w = Printf.sprintf "\\item %s" (Path.show_history (fun s -> s) w) in
        let sorted = Util.reverse_tr (List.sort compare_by_weight ways_to_cycle) in
        let ways_in_lines = ["Ways to this cycle";"\\begin{itemize}"] @ (Util.map_tr (fun w -> (verbatim (summarise w)) ^ "\\\\") sorted) @ ["\\end{itemize}"] in
        let middle = [tikz_diagram] @ [""] @ ways_in_lines in

        let finish = ["\\end{figure}"] in
        start @ middle @ finish

(*
 * First argument:              a wmcfg grammar file
 * Second argument (optional):  a guillaumin-generated dict file
 *)
let main () =
        let grammar_file =
                if (Array.length Sys.argv > 1) then
                        Sys.argv.(1)
                else
                        failwith "Need a grammar file on the command line"
        in
        let dict_file =
                if (Array.length Sys.argv > 2) then
                        Some Sys.argv.(2)
                else
                        None
        in
        let (rules, start_symbol) = Grammar.get_input_grammar grammar_file in
        let dict = match dict_file with None -> None | Some f -> Some (Grammar.get_guillaumin_dict f) in
        let broken_paths = find_cycles rules start_symbol in    (* a broken path is a pair of histories, the first of which is a cycle *)
        let sort xs = Util.reverse_tr (List.sort (fun (c1,_) (c2,_) -> compare_by_weight c1 c2) xs) in
        let cycles = sort (group broken_paths) in
        let cycles' = Util.map_tr fst cycles in   (* Just the cycles themselves, no "ways in" *)
        prerr_endline (String.concat "\n" (Util.map_tr (Path.show_history (fun x -> x)) cycles')) ;
        let prerr_cycle (cycle, ways_to_cycle) =
                prerr_endline "===============" ;
                prerr_endline (Path.show_history (fun x -> x) cycle) ;
                prerr_endline "    Ways into this cycle:" ;
                List.iter (fun w -> prerr_endline ("    " ^ (Path.show_history (fun x -> x) w))) ways_to_cycle ;
                prerr_endline "===============" ;
                () in
        List.iter prerr_cycle cycles ;
        print_endline (String.concat "\n" latex_start) ;
        print_endline "%\\section{Weights of cycles}" ;
        print_endline "%\\begin{enumerate}" ;
        print_endline (String.concat "\n" (Util.map_tr ((^) "%\\item ") (Util.map_tr (fun h -> Util.show_weight_float (Path.weight_product h)) cycles'))) ;
        print_endline "%\\end{enumerate}" ;
        print_endline "%\\clearpage" ;
        print_endline "%\\section{Trees of cycles}" ;
        List.iter (fun (c,ws) -> (List.iter print_endline (latex_cycle dict (c,ws)))) cycles ;
        print_endline (String.concat "\n" latex_end)

let _ = if (!Sys.interactive) then () else main ()

