open Util

type matrix = float array array * string list

exception IndexingError of (string * string list)

exception NonInvertible of matrix

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

let invert (m,indices) =
    let dim = List.length indices in
    let make_augmented_row ri = Array.init (2 * dim) (fun ci -> if ci < dim then m.(ri).(ci) else if ci = ri + dim then 1.0 else 0.0) in
    let augmented_matrix = Array.init dim make_augmented_row in

    (* Modify a row according to the given function, which accepts a column index and the current element as arguments. *)
    let tweak_row m ri (f : int -> float -> float) =
        let tweak_position ci =
            let old = m.(ri).(ci) in
            Array.set m.(ri) ci (f ci old)
        in
        List.iter tweak_position (range 0 (2*dim))
    in

    let swap_rows m ri1 ri2 =
        let old_ri1 = m.(ri1) in
        m.(ri1) <- m.(ri2) ;
        m.(ri2) <- old_ri1
    in

    (* Modify augmented_matrix to have a zero at position (ri,ci) by adding some multiple of row rownum to row ri *)
    let zero_position_using_row (ri,ci) rownum =
        let here = augmented_matrix.(ri).(ci) in
        if here <> 0.0 then (
            let there = augmented_matrix.(rownum).(ci) in
            assert (there <> 0.0) ;
            let factor = here /. there in
            tweak_row augmented_matrix ri (fun ci' x -> x -. factor *. augmented_matrix.(rownum).(ci'))
        ) else ()
    in

    (* Manipulate the augmented matrix, column by column working from the left, into a form where:
        (a) everything underneath the diagonal is zero, and 
        (b) everything on the diagonal is non-zero. *)
    let zero_bottom_part_of_col ci =
        if augmented_matrix.(ci).(ci) = 0.0 then
            let values_in_this_col = map_tr (fun i -> (i, augmented_matrix.(i).(ci))) (range (ci+1) dim) in
            match (List.filter (fun (i,x) -> x <> 0.0) values_in_this_col) with
            | [] -> raise (NonInvertible (m,indices))
            | ((otherrow,_)::_) -> swap_rows augmented_matrix otherrow ci
        else () ;
        List.iter (fun ri -> zero_position_using_row (ri,ci) ci) (range (ci+1) dim)
    in
    List.iter zero_bottom_part_of_col (range 0 dim) ;

    (* Zero out the top right triangle of the matrix, column by column working from the right *)
    let zero_top_part_of_col ci = List.iter (fun ri -> zero_position_using_row (ri,ci) ci) (reverse_tr (range 0 ci)) in
    List.iter zero_top_part_of_col (range 0 dim) ;

    (* Scale each row to make the values on the diagonal one. *)
    let set_diagonal_to_one i =
        let z = augmented_matrix.(i).(i) in
        assert (z <> 0.0) ;
        tweak_row augmented_matrix i (fun ci x -> x /. z)
    in
    List.iter set_diagonal_to_one (range 0 dim) ;

    let extract_inverse_row ri = Array.sub augmented_matrix.(ri) dim dim in
    let result = Array.init dim extract_inverse_row in

    (result,indices)

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


(******************************************************************************************)

(* functions created by Angelica *)

let div_vec_by v scalar = 
    List.map (fun x -> x /. scalar) v

let get_matrix (m, indices) = m

let modify_element (m,indices) row_index col_index new_value =
    let row_num = index_as_int indices row_index in
    let col_num = index_as_int indices col_index in 
    m.(row_num).(col_num) <- new_value

let spectral_radius (m, indices) =
    let d = List.length(indices) in
    (* initial spectral radius and corresponding eigenvector approximation *)
    let v_0 = Util.map_tr (fun x -> 1. /. sqrt (float_of_int d)) (Util.range 0 d) in
    let ev_0 = 0. in 

    (* power method implementation based on this video [https://www.youtube.com/watch?v=yBiQh1vsCLU]
    and this python implementation [http://mlwiki.org/index.php/Power_Iteration] *)
    let rec power_iterate matrix v_current ev_current =
        let p = mult_by_vec matrix v_current in
        let abs_max lst =
            let abs_lst = List.map abs_float lst in
            match abs_lst with
            | [] -> invalid_arg "empty list"
            | x::xs -> List.fold_left max x xs
        in 
        let ev_next = abs_max p in
        let v_next = div_vec_by p ev_next in 
        if abs_float(ev_next -. ev_current) < 0.00001
        then ev_next
        else power_iterate matrix v_next ev_next
    in power_iterate (m, indices) v_0 ev_0
    
(* Angelica (5/20): added various things *)
let print ?(ch = stdout) (m,indices) =
    Printf.printf ("Spectral radius: %.*F \n\n") 5 (spectral_radius (m, indices)) ;

    Printf.printf("Here are the indices:\n") ;
    List.iter (fun s -> Printf.printf "%s " s) indices ;
    Printf.printf("\n\n") ;

    Printf.printf("Here is the fertility matrix:\n") ;
    (* returns length of longest string in [indices] *)
    let rec longest_string lst = 
        match lst with
        | [] -> 0
        | x::[] -> String.length x
        | x::y::z ->  if String.length x > String.length y then longest_string (x::z) else longest_string (y::z)
    in
    (* [spacing] determines the number of fractional digits ; used to keep the printed matrix evenly-spaced *)
    let spacing = max (longest_string indices) 2 in 
    let print_row r =  
        Printf.fprintf ch "%-*s | " spacing r;
        List.iter (fun c -> Printf.fprintf ch "%.*f " spacing (get_element (m,indices) r c)) indices ;
        Printf.fprintf ch "|\n"
    in
    (* prints indices horizontally *)
    Printf.fprintf ch "%-*s" (spacing + 3) "" ;
    List.iter (fun x -> Printf.fprintf ch "%-*s " (spacing + 2) x) indices ;
    Printf.printf("\n") ;

    (* prints rest of matrix *)
    List.iter print_row indices ;
    Printf.printf("\n")


let create_test_matrix n lst str_lst = 
    assert (List.length lst = n*n) ;

    (* create (n x n) matrix with uninitialized values *)
    let skeleton = Array.init n (fun (t:int) -> Array.create_float n) in
    (* modify_row : float array -> float list -> int -> (matrix * float list) 
        Modifies a row of [skeleton] using the first n elements of [float list], and removes said elements from [float list]. *)
    let rec modify_row row float_list col_num =
        if col_num = n
        then (() ; (skeleton, float_list))
        else (
            Array.set row col_num (List.hd float_list) ;
            modify_row row (List.tl float_list) (col_num + 1)
        )
    in 
    (* modify_matrix : matrix -> float list -> i:int -> matrix 
        Calls [modify_row] on a given [matrix] and [float list], [i] times. *)
    let rec modify_matrix matrix float_list i =
        if i = n 
        then
            (* If the [string list] argument for [create_test_matrix] is [], use ASCII characters from "A" as indices. *)
            if (str_lst = []) then  
                let ascii_code_list length = List.map (Char.chr) (Util.range 65 (65 + length)) in
                let indices = List.map (Char.escaped) (ascii_code_list n) in (skeleton, indices)
            (* Else use the [string list] argument as indices. *)
            else (
                assert (List.length str_lst = n) ;
                let indices = str_lst in (skeleton, indices)
            )
        else
            let (_, new_list) = modify_row skeleton.(i) float_list 0 in
            modify_matrix skeleton new_list (i + 1)
    in modify_matrix skeleton lst 0


(* matrices for testing *)
(* let test_a = create_test_matrix 4 [0.;1.;0.;0.;0.;0.;0.5;0.;0.;0.;0.5;1.;0.;0.;0.5;0.] []
let test_b = create_test_matrix 3 [0.;1.;0.;0.;0.5;1.;0.;0.5;0.5] [] 
let test_c = create_test_matrix 2 [0.;1.;0.;1.] [] 
let test_d = create_test_matrix 4 [0.;1.;0.;0.;0.;0.;0.;0.5;0.;0.;0.5;0.5;0.;0.;0.5;0.] []  *)

(* results of Matrix.spectral_radius / as calculated by [https://www.symbolab.com/solver/matrix-eigenvalues-calculator/]*)
(* spectral_radius test_a = 0.999997138982507749   (1.0)
spectral_radius test_b = 1.20710571923743504    (≈1.207106781186)
spectral_radius test_c = 1.0                    (1.0)
spectral_radius test_d = 0.80901856763925728    (≈0.8090169943749) *)
