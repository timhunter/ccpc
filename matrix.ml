open Util

type matrix = OCamlMatrix.Matrix.EltMatrix.matrix * string list

exception IndexingError of (string * string list)

(************************************************************)
(* Private functions for use inside this file ***************)

let elt_of_float = OCamlMatrix.Elts.Elts.from_float
let float_of_elt = OCamlMatrix.Elts.Elts.to_float

let index_as_int indices str =
    match find_in_list str indices with
    | (n::[]) -> n + 1
    | _ -> raise (IndexingError (str, indices))

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

let get_indices (m,indices) = indices

let identity_matrix xs =
    create_square_matrix xs (fun r c -> if r = c then 1.0 else 0.0)

let invert (m,indices) =
    (OCamlMatrix.Matrix.EltMatrix.inverse m, indices)

let multiply (m1,indices1) (m2,indices2) =
    assert (indices1 = indices2) ;
    (OCamlMatrix.Matrix.EltMatrix.mult m1 m2, indices2)

let add (m1,indices1) (m2,indices2) =
    assert (indices1 = indices2) ;
    (OCamlMatrix.Matrix.EltMatrix.add m1 m2, indices2)

let subtract (m1,indices1) (m2,indices2) =
    let neg_m2 = OCamlMatrix.Matrix.EltMatrix.scale m2 (OCamlMatrix.Elts.Elts.from_float (-1.0)) in
    add (m1,indices1) (neg_m2,indices2)

let mult_vec_by xs (m,_) =
    let indices = map_tr (fun x -> x + 1) (range 0 (List.length xs)) in
    let row_vector = OCamlMatrix.Matrix.EltMatrix.from_list [map_tr OCamlMatrix.Elts.Elts.from_float xs] in
    let m_result = OCamlMatrix.Matrix.EltMatrix.mult row_vector m in
    map_tr (fun c -> OCamlMatrix.Elts.Elts.to_float (OCamlMatrix.Matrix.EltMatrix.get_elt m_result (1,c))) indices

let mult_by_vec (m,_) xs =
    let indices = map_tr (fun x -> x + 1) (range 0 (List.length xs)) in
    let col_vector = OCamlMatrix.Matrix.EltMatrix.from_list (map_tr (fun x -> [OCamlMatrix.Elts.Elts.from_float x]) xs) in
    let m_result = OCamlMatrix.Matrix.EltMatrix.mult m col_vector in
    map_tr (fun r -> OCamlMatrix.Elts.Elts.to_float (OCamlMatrix.Matrix.EltMatrix.get_elt m_result (r,1))) indices

let print (m,_) =
    let (num_rows, num_cols) = OCamlMatrix.Matrix.EltMatrix.get_dimensions m in
    let go r c elt =
        if c = 1 then Printf.printf "| " else () ;
        Printf.printf "% f " (OCamlMatrix.Elts.Elts.to_float elt) ;
        if c = num_cols then Printf.printf "|\n" else () ;
    in
    OCamlMatrix.Matrix.EltMatrix.iteri go m

