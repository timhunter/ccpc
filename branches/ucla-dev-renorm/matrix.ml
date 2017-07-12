open Util

type matrix = float array array

let create_square_matrix n f =
    let result = Array.make_matrix n n 0.0 in
    let indices = range 0 n in
    let fill_row r = List.iter (fun c -> Array.set result.(r) c (f r c)) indices in
    List.iter fill_row indices ;
    result

let invert m =
    m
