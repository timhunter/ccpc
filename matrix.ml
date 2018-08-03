open Util

type matrix = OCamlMatrix.Matrix.EltMatrix.matrix * string list

exception IndexingError of (string * string list)

(************************************************************)
(* Private functions for use inside this file ***************)

let index_as_int indices str =
    match find_in_list str indices with
    | (n::[]) -> n + 1
    | _ -> raise (IndexingError (str, indices))

let dot_product xs ys =
    List.fold_left (+.) 0.0 (List.map2 ( *. ) xs ys)

(************************************************************)

let create_square_matrix xs f =
    let create_row r = map_tr (fun c -> OCamlMatrix.Elts.Elts.from_float (f r c)) xs in
    let list_of_rows = map_tr create_row xs in
    let m = OCamlMatrix.Matrix.EltMatrix.from_list list_of_rows in
    (m, xs)

let get_element (m,indices) r c =
    let row_num = index_as_int indices r in
    let column_num = index_as_int indices c in
    OCamlMatrix.Elts.Elts.to_float (OCamlMatrix.Matrix.EltMatrix.get_elt m (row_num, column_num))

let get_row (m,indices) r =
    map_tr (fun c -> get_element (m,indices) r c) indices

let get_col (m,indices) c =
    map_tr (fun r -> get_element (m,indices) r c) indices

let get_indices (m,indices) = indices

let identity_matrix xs =
    create_square_matrix xs (fun r c -> if r = c then 1.0 else 0.0)

let invert (m,indices) =
    (OCamlMatrix.Matrix.EltMatrix.inverse m, indices)

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

