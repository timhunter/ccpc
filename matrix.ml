open Util

type matrix = float array array * string list

exception IndexingError of (string * string list)

(************************************************************)
(* Private functions for use inside this file ***************)

let index_as_int indices str =
    match find_in_list str indices with
    | (n::[]) -> n
    | _ -> raise (IndexingError (str, indices))

let dot_product xs ys =
    List.fold_left (+.) 0.0 (List.map2 ( *. ) xs ys)

(************************************************************)

let create_square_matrix indices f =
    let dim = List.length indices in
    let create_row (r : string) : float array = Array.init dim (fun ci -> f r (List.nth indices ci)) in
    let m = Array.init dim (fun ri -> create_row (List.nth indices ri)) in
    (m, indices)

let get_element (m,indices) r c =
    let row_num = index_as_int indices r in
    let column_num = index_as_int indices c in
    try
        m.(row_num).(column_num)
    with e ->
        Printf.eprintf "Indexing error trying to look up index (%d,%d), from strings (%s,%s), in this matrix:\n" row_num column_num r c ;
        Array.iter (fun row -> Array.iter (fun x -> Printf.eprintf "% f " x) row; Printf.eprintf "\n") m ;
        raise e

let get_row (m,indices) r =
    map_tr (fun c -> get_element (m,indices) r c) indices

let get_col (m,indices) c =
    map_tr (fun r -> get_element (m,indices) r c) indices

let get_indices (m,indices) = indices

let identity_matrix xs =
    create_square_matrix xs (fun r c -> if r = c then 1.0 else 0.0)

let invert ((m,indices) : matrix) =
    let m' = OCamlMatrix.Matrix.EltMatrix.from_list (map_tr (fun ri -> map_tr OCamlMatrix.Elts.Elts.from_float (get_row (m,indices) ri)) indices) in
    let invm' = OCamlMatrix.Matrix.EltMatrix.inverse m' in
    let invm = create_square_matrix indices (fun r c -> let ri = index_as_int indices r + 1 in
                                                        let ci = index_as_int indices c + 1 in
                                                        let elt = OCamlMatrix.Matrix.EltMatrix.get_elt invm' (ri,ci) in
                                                        OCamlMatrix.Elts.Elts.to_float elt) in
    invm

let multiply (m1,indices1) (m2,indices2) =
    assert (indices1 = indices2) ;
    let f r c = dot_product (get_row (m1,indices1) r) (get_col (m2,indices2) c) in
    create_square_matrix indices2 f

let add (m1,indices1) (m2,indices2) =
    assert (indices1 = indices2) ;
    let f r c = get_element (m1,indices1) r c +. get_element (m2,indices2) r c in
    create_square_matrix indices2 f

let subtract (m1,indices1) (m2,indices2) =
    let neg_m2 = create_square_matrix indices2 (fun r c -> -1.0 *. get_element (m2,indices2) r c) in
    add (m1,indices1) neg_m2

let mult_vec_by xs (m,indices) =
    assert (List.length xs = List.length indices) ;
    map_tr (fun c -> dot_product xs (get_col (m,indices) c)) indices

let mult_by_vec (m,indices) xs =
    assert (List.length xs = List.length indices) ;
    map_tr (fun r -> dot_product (get_row (m,indices) r) xs) indices

let print ?(ch = stdout) (m,indices) =
    let print_row r =
        Printf.fprintf ch "| " ;
        List.iter (fun c -> Printf.fprintf ch "% f " (get_element (m,indices) r c)) indices ;
        Printf.fprintf ch "|\n"
    in
    List.iter print_row indices

