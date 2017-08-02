open Util

type matrix = OCamlMatrix.Matrix.EltMatrix.matrix

(************************************************************)
(* Private functions for use inside this file ***************)

let elt_of_float x = OCamlMatrix.Elts.Elts.from_string (string_of_float x)

let float_of_elt x = float_of_string (OCamlMatrix.Elts.Elts.to_string x)

(************************************************************)

let create_square_matrix n f =
    let indices = range 0 n in
    let create_row r = map_tr (fun c -> elt_of_float (f r c)) indices in
    let list_of_columns = map_tr create_row indices in
    OCamlMatrix.Matrix.EltMatrix.from_list list_of_columns

let invert = OCamlMatrix.Matrix.EltMatrix.inverse

let multiply = OCamlMatrix.Matrix.EltMatrix.mult

let mult_vec_by xs m =
    let row_vector = OCamlMatrix.Matrix.EltMatrix.from_list [map_tr elt_of_float xs] in
    multiply row_vector m

let mult_by_vec m xs =
    let col_vector = OCamlMatrix.Matrix.EltMatrix.from_list (map_tr (fun x -> [elt_of_float x]) xs) in
    multiply m col_vector

let print m =
    let (num_rows, num_cols) = OCamlMatrix.Matrix.EltMatrix.get_dimensions m in
    let go r c elt =
        if c = 1 then Printf.printf "| " else () ;
        Printf.printf "% f " (float_of_elt elt) ;
        if c = num_cols then Printf.printf "|\n" else () ;
    in
    OCamlMatrix.Matrix.EltMatrix.iteri go m

