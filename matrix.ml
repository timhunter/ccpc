open Util

type matrix = OCamlMatrix.Matrix.EltMatrix.matrix

let create_square_matrix n f =
    let indices = range 0 n in
    let elt_of_float x = OCamlMatrix.Elts.Elts.from_string (string_of_float x) in   (* what a mess *)
    let create_row r = map_tr (fun c -> elt_of_float (f r c)) indices in
    let list_of_columns = map_tr create_row indices in
    OCamlMatrix.Matrix.EltMatrix.from_list list_of_columns

let invert = OCamlMatrix.Matrix.EltMatrix.inverse

