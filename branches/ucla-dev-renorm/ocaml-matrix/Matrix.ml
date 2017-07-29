open Order
open Elts

(* Functor so we can Abstract away! *)
module MakeMatrix (C: EltsI.ORDERED_AND_OPERATIONAL) : 
  (MatrixI.MATRIX with type elt = C.t) =
struct

  (*************** Exceptions ***************)
  
  exception NonSquare
  exception ImproperDimensions

  (*************** End Exceptions ***************)
  
  (*************** Types ***************)

  type elt = C.t
  
  (* A matrix is a pair of dimension (n x p) and a array of arrays
   * the first array is the row (n) and the second the column (p) *)
  type matrix = (int * int) * (elt array array) 

  (*************** End Types ***************)

  (*************** Base Functions ***************)

  (* catching negative dimensions AND 0 dimensions and too large 
   * of a dimension so we don't have to worry about it later *)
  let empty (rows: int) (columns: int) : matrix = 
    if rows > 0 && columns > 0 then
      try
        let m = Array.make_matrix rows columns C.zero in ((rows,columns),m)
      with Invalid_argument "index out of bounds" ->
        raise ImproperDimensions
    else (* dimension is negative or 0 *)
      raise ImproperDimensions

  (*************** End Base Functions ***************)

  (*************** Helper Functions ***************)

  (* get's the nth row of a matrix and returns (r, row) where r is the length
   * of the row and row is a COPY of the original row. For example, calling
   * calling get_row m 1 will return (3, |1 3 4 |) 
   *         ________
   *    m = | 1 3 4 | 
   *        |*2 5 6 | 
   *)
  (* aside: we don't check whether n < 1 because of our matrix invariant *)
  let get_row (((n,p),m): matrix) (row: int) : int * elt array =
    if row <= n then 
      let row' = Array.map (fun x -> x) m.(row - 1) in
      (p, row')
    else 
      raise (Failure "Row out of bounds.")

  (* similar to get_row. For m, get_column m 1 will return (2, |1 2|) *)
  let get_column (((n,p),m): matrix) (column: int) : int * elt array =
    if column <= p then 
      begin
        let column' = Array.make n C.zero in
        for i = 0 to n - 1 do
          column'.(i) <- m.(i).(column - 1)
        done;
        (n, column')
      end
    else
      raise (Failure "Column out of bounds.")

  (* sets the nth row of the matrix m to the specified array a. 
   * This is done IN-PLACE. Therefore the function returns unit.  You should
   * nonetheless enfore immutability whenever possible.  For a clarification on
   * what nth row means, look at comment for get_row above. *)
  let set_row (((n,p),m): matrix) (row: int) (a: elt array) : unit =
    if row <= n then 
    begin
      assert(Array.length a = p);
      for i = 0 to p - 1 do
        m.(row - 1).(i) <- a.(i)
      done;
    end
    else
      raise (Failure "Row out of bounds.")

  (* Similar to set_row but sets the nth column instead *)
  let set_column (((n,p),m): matrix) (column: int) (a: elt array) : unit =
    if column <= p then
    begin
      assert(Array.length a = n);
      for i = 0 to n - 1 do
        m.(i).(column - 1) <- a.(i)
      done; 
    end
    else
      raise (Failure "Column out of bounds.")

  (* returns the ij-th element of a matrix (not-zero indexed) *)
  let get_elt (((n,p),m): matrix) ((i,j): int*int) : elt =
    if i <= n && j <= p then
      m.(i - 1).(j - 1)
    else 
      raise ImproperDimensions

  (* Changes the i,j-th element of a matrix to e. Is not zero-indexed, and
   * changes the matrix in place *)
  let set_elt (((n,p),m): matrix) ((i,j): int*int) (e: elt) : unit =
    if i <= n && j <= p then
      m.(i - 1).(j - 1) <- e 
    else 
      raise ImproperDimensions

  (* similar to map, but applies to function to the entire matrix 
   * Returns a new matrix *)
  let map (f: elt -> elt) (mat: matrix) : matrix =
    let (dim,m) = mat in 
    (dim, Array.map (Array.map f) m)

  (* Just some wrapping of Array.iter made for Matrices! *)
  let iter (f: elt -> unit) (mat: matrix) : unit =
    let dim,m = mat in
    Array.iter (Array.iter f) m

  (* Just some wrapping of Array.iteri. Useful for pretty
   * printing matrix. The index is (i,j). NOT zero-indexed *)
  let iteri (f: int -> int -> elt -> unit) (mat: matrix) : unit =
    let dim,m = mat in
    Array.iteri (fun i row -> Array.iteri (fun j e -> f (i+1) (j+1) e) row) m 

  (* folds over each row using base case u and function f *)
  (* could be a bit more efficient? *)
  let reduce (f: 'a -> elt -> 'a) (u: 'a) (((p,q),m): matrix) : 'a =
    let total = ref u in
      for i = 0 to p - 1 do
        for j = 0 to q - 1 do
          total := f (!total) m.(i).(j) 
        done;
      done;
    !total

  (* given two arrays, this will calculate their dot product *)
  (* It seems a little funky, but this is done for efficiency's sake.
   * In short, it tries to multiply each element by it's respective
   * element until the one array is indexed out of bounds. If the
   * other array is also out of bounds, then it returns their value.
   * Otherwise, the arrays were the wrong size and raises ImproperDimension

    THE ABOVE COMMENT HAS NOT BEEN IMPLEMENTED 

    Instead we calculate the length before starting
   *)
  let dot (v1: elt array) (v2: elt array) : elt =
    let rec dotting (i: int) (total: elt) : elt =
      if i = 0 then total
      else 
        let curr = C.multiply v1.(i-1) v2.(i-1) in
        dotting (i - 1) (C.add curr total) in
    let len1, len2 = Array.length v1, Array.length v2 in
    if len1 = len2 then dotting len1 C.zero
    else raise ImproperDimensions

  (* function to expose the dimensions of a matrix *)
  let get_dimensions (m: matrix) : (int * int) =
    let ((x,y),m') = m in (x,y)
    
  (*************** End Helper Functions ***************)


  (*************** Primary Matrix Functions ***************)
      
  (* scales a matrix by the appropriate factor *)
  let scale (m: matrix) (sc: elt) : matrix = map (C.multiply sc) m

  (* Generates a matrix from a list of lists. The inners lists are the rows *)
  let from_list (lsts : elt list list) : matrix = 
    let rec check_length (length: int) (lst: elt list) : int =
      if List.length lst = length then length
      else raise ImproperDimensions in 
    let p = List.length lsts in
    match lsts with
    | [] -> raise ImproperDimensions
    | hd::tl -> 
      let len = List.length hd in
      if List.fold_left check_length len tl = len then
        ((p,len),Array.map Array.of_list (Array.of_list lsts))
      else
        raise ImproperDimensions

  (* Adds two matrices. They must have the same dimensions *)
  let add ((dim1,m1): matrix) ((dim2,m2): matrix) : matrix =
    if dim1 = dim2 then
      let n, p = dim1 in
      let (dim', sum_m) = empty n p in
      for i = 0 to n - 1 do
        for j = 0 to p - 1 do
          sum_m.(i).(j) <- C.add m1.(i).(j) m2.(i).(j)
        done;
      done;
      (dim',sum_m)
    else
      raise ImproperDimensions
  

  (* Multiplies two matrices. If the matrices have dimensions m x n and p x q, n
   * and p must be equal, and the resulting matrix will have dimension n x q *)
  let mult (matrix1: matrix) (matrix2: matrix) : matrix =  
    let ((m,n),m1), ((p,q),m2) = matrix1, matrix2 in
    if n = p then
      let (dim, result) = empty m q in
      for i = 0 to m - 1 do
        for j = 0 to q - 1 do
          let (_,row), (_,column) = get_row matrix1 (i + 1), 
            get_column matrix2 (j + 1) in
          result.(i).(j) <- dot row column
        done;
      done;
      (dim,result)
    else
      raise ImproperDimensions

  (*************** Helper Functions for Row Reduce ***************)

  (* returns the index of the first non-zero elt in an array*)
  let zero (arr: elt array) : int option =
    let index = ref 1 in
    let empty (i: int option) (e: elt) : int option =
      match i, C.compare e C.zero with
      | None, Equal -> (index := !index + 1; None)
      | None, _ -> Some (!index) 
      | _, _ -> i in
    Array.fold_left empty None arr

  (* returns the the location of the nth non-zero
   * element in the matrix. Scans column wise.  So the nth non-zero element is
   * the FIRST non-zero element in the nth non-zero column *)
  let nth_nz_location (m: matrix) (n: int): (int*int) option =
    let ((n,p),m') = m in
    let rec check_col (to_skip: int) (j: int) =
      if j <= p then
        let (_,col) = get_column m j in
        match zero col with
        | None -> check_col to_skip (j + 1)
        | Some i -> 
          if to_skip = 0 then 
            Some (i,j)
          else (* we want a later column *) 
            check_col (to_skip - 1) (j + 1)
      else None in
    check_col (n - 1) 1

  (* returns the the location of the first
   * non-zero and non-one elt. Scans column wise, from
   * left to right. Basically, it ignores columns
   * that are all zero or that *)
  let fst_nz_no_loc (m: matrix): (int*int) option =
    let ((n,p),m') = m in
    let rec check_col (j: int) =
      if j <= p then
        let (_,col) = get_column m j in
        match zero col with
        | None -> check_col (j + 1)
        | Some i -> 
          match C.compare col.(i-1) C.one with
          | Equal -> check_col (j + 1)
          | _ -> Some (i,j)
      else None in
    check_col 1

  (* Compares two elements in an elt array and returns the greater and its
   * index. Is a helper function for find_max_col_index *)
  let compare_helper (e1: elt) (e2: elt) (ind1: int) (ind2: int) : (elt*int) = 
    match C.compare e1 e2 with 
    | Equal -> (e2, ind2)
    | Greater -> (e1, ind1)
    | Less -> (e2, ind2) 

  (* Finds the element with the greatest absolute value in a column. Is not 
   * 0-indexed. If two elements are both the maximum value, returns the one with
   * the lowest index. Returns None if this element is zero (if column is all 0)
   *)
  let find_max_col_index (array1: elt array) (start_index: int) : int option = 
    let rec find_index (max_index: int) (curr_max: elt) (curr_index: int) 
        (arr: elt array) = 
      if curr_index = Array.length arr then 
        (if curr_max = C.zero then None
        else Some (max_index+1)) (* Arrays are 0-indexed but matrices aren't *)
      else
        (match C.compare arr.(curr_index) C.zero with
        | Equal -> find_index max_index curr_max (curr_index+1) arr
        | Greater -> 
          (let (el, index) = compare_helper (arr.(curr_index)) 
            curr_max curr_index max_index in
          find_index index el (curr_index+1) arr)
        | Less -> 
          (let abs_curr_elt = C.subtract C.zero arr.(curr_index) in
          let (el, index) = compare_helper abs_curr_elt curr_max curr_index
            max_index in
          find_index index el (curr_index+1) arr))
    in
    find_index 0 C.zero (start_index -1) array1

  (* Basic row operations *)
  (* Scales a row by sc *)
  let scale_row (m: matrix) (num: int) (sc: elt) : unit = 
    let (len, row) = get_row m num in 
    let new_row = Array.map (C.multiply sc) row in
    set_row m num new_row

  (* Swaps two rows of a matrix *)
  let swap_row (m: matrix) (r1: int) (r2: int) : unit =
    let (len1, row1) = get_row m r1 in
    let (len2, row2) = get_row m r2 in
    let _ = assert (len1 = len2) in 
    let _ = set_row m r1 row2 in 
    let _ = set_row m r2 row1 in
    ()

  (* Subtracts a multiple of r2 from r1 *)
  let sub_mult (m: matrix) (r1: int) (r2: int) (sc: elt) : unit = 
    let (len1, row1) = get_row m r1 in
    let (len2, row2) = get_row m r2 in
    let _ = assert (len1 = len2) in
    for i = 0 to len1 - 1 do (* Arrays are 0-indexed *)
      row1.(i) <- C.subtract row1.(i) (C.multiply sc row2.(i))
    done;
    set_row m r1 row1 

  (*************** End Helper Functions for Row Reduce ***************)

  (* Returns the row reduced form of a matrix. Is not done in place, but creates
   * a new matrix *)
  let row_reduce (mat: matrix) : matrix =
    let rec row_reduce_h (n_row: int) (n_col: int) (mat2: matrix) : unit = 
      let ((num_row, num_col), arr) = mat2 in
      if (n_col = num_row + 1) then ()
      else
        let (_,col) = get_column mat2 n_col in
        match find_max_col_index col n_row with
        | None (* Column all 0s *) -> row_reduce_h n_row (n_col+1) mat2 
        | Some index -> 
          begin
            swap_row mat2 index n_row;
            let pivot = get_elt mat2 (n_row, n_col) in
            scale_row mat2 (n_row) (C.divide C.one pivot);
            for i = 1 to num_row do
              if i <> n_row then sub_mult mat2 i n_row (get_elt mat2 (i,n_col))
            done;
            row_reduce_h (n_row+1) (n_col+1) mat2
          end
    in
    (* Copies the matrix *)
    let ((n,p),m) = mat in
    let (dim,mat_cp) = empty n p in
    for i = 0 to n - 1 do
      for j = 0 to p - 1 do
        mat_cp.(i).(j) <- m.(i).(j)
      done;
    done;
    let _ = row_reduce_h 1 1 (dim,mat_cp) in (dim,mat_cp)

   (* Pretty prints a matrix: mostly used for testing *)
   let print (m: matrix) : unit =
    let ((row,col), m') = m in
    let pretty_print (_: int) (j: int) (e: elt) =
      if j = 1 then
        print_string "|"
      else ();
      C.print e;
      print_char ' ';
      if j = col then
        print_string "|\n"
      else () in
    iteri pretty_print m

  (*************** End Main Functions ***************)

  (************** Input and Output Functions **********)
  let rec read_data (chan: in_channel) : elt list list =  
    try
      let row = input_line chan in
      let chars = Helpers.explode row "," in
      (List.map C.from_string chars)::read_data chan
     with End_of_file -> []

  (* Load a matrix from a file with filename s *)
  let load (s: string): matrix =
    try
      let chan = open_in s in
      let m_list = read_data chan in
      let matrix = from_list m_list in
      close_in chan; matrix
    with
    | e -> raise e 

  let write_data (((row,col),m): matrix) : string =
    let buffer = ref "" in
    let to_string (_: int) (j: int) (e: elt) =
      buffer := !buffer ^ C.to_string e;
      if j = col then
        buffer := !buffer ^ "\n"
      else 
        buffer := !buffer ^ "," 
      in
    iteri to_string ((row,col),m);
    !buffer

  (* Dumps the matrix to a file with filename name *)
  let dump (name: string) (m: matrix) : unit =
    try 
      let outchan = open_out name in
      let s = write_data m in
      output_string outchan s;
      close_out outchan
    with
      | Sys_error e -> print_string e 




  (*************** Optional module functions ***************)

  (* calculates the trace of a matrix *)
  let trace (((n,p),m): matrix) : elt =
    let rec build (elt: elt) (i: int) =
      if i > -1 then
        build (C.add m.(i).(i) elt) (i - 1)
      else
        elt in
    if n = p then build C.zero (n - 1)
    else raise ImproperDimensions 

  (* calculates the transpose of a matrix and retuns a new one *)
  let transpose (((n,p),m): matrix) =
    let (dim,m') = empty p n in
    for i = 0 to n - 1 do
      for j = 0 to p - 1 do
        m'.(j).(i) <- m.(i).(j)
      done;
    done;
    assert(dim = (p,n));
    ((p,n),m')

  (* Returns the inverse of a matrix. Uses a pretty simple algorithm *)
  let inverse (mat: matrix) : matrix =
    let ((n,p),m) = mat in
    if n = p then
      (* create augmented matrix *)
      let augmented = empty n (2*n) in
      for i = 1 to n do
        let (dim,col) = get_column mat i in
        let arr = Array.make n C.zero in
        begin
          assert(dim = n);
          arr.(i-1) <- C.one;
          set_column augmented i col;
          set_column augmented (n + i) arr
        end
      done;
      let augmented' = row_reduce augmented in
      (* create the inverted matrix and fill in with appropriate values *)
      let inverse = empty n n in
      for i = 1 to n do
        let (dim, col) = get_column augmented' (n + i) in
        let _ = assert(dim = n) in
        let _ = set_column inverse i col in
        ()
      done;
      inverse
    else
      raise NonSquare

  (* following our invarient, converts a string into a matrix *)
  let from_string (s: string) : matrix =
    let convert (row: string) : elt list =
      let chars = Helpers.explode row "," in
      List.map C.from_string chars in
    let rows = Helpers.explode s "|" in
    let char_list = List.map convert rows in
    from_list char_list

  (***************** HELPER FUNCTIONS FOR DETERMINANT *****************)
  (* creates an identity matrix of size n*)
  let create_identity (n:int) : matrix = 
    let (dim,m) = empty n n in
    for i = 0 to n - 1 do
      m.(i).(i) <- C.one
    done;
    (dim,m)

  (* Finds the index of the maximum value of an array *)
  let find_max_index (arr: elt array) (start_index : int) : int =
    let rec find_index (max_index: int) (curr_index: int) = 
      if curr_index = Array.length arr then max_index+1
      else
        match C.compare arr.(curr_index) arr.(max_index) with
        | Equal | Less -> find_index max_index (curr_index + 1)
        | Greater -> find_index curr_index (curr_index + 1) in
    find_index (start_index - 1) start_index

  (* Creates the pivoting matrix for A. Returns swqps. Adapted from 
   * http://rosettacode.org/wiki/LU_decomposition#Common_Lisp *)
  let pivotize (((n,p),m): matrix) : matrix * int =
    if n = p then
      let swaps = ref 0 in
      let pivot_mat = create_identity n in
      for j = 1 to n do
        let (_,col) = get_column ((n,p),m) j in
        let max_index = find_max_index col j in
        if max_index <> j then 
          (swaps := !swaps + 1; swap_row pivot_mat max_index j)
        else ()
      done; 
      (pivot_mat,!swaps)
    else raise ImproperDimensions

  (* decomposes a matrix into a lower triangualar, upper triangualar 
   * and a pivot matrix. It returns (L,U,P). Adapted from 
   * http://rosettacode.org/wiki/LU_decomposition#Common_Lisp *)
  let lu_decomposition (((n,p),m): matrix) : (matrix*matrix*matrix)*int =
    if n = p then
      let mat = ((n,p),m) in
      let lower, upper, (pivot,s) = empty n n, empty n n, pivotize mat in
      let ((x1,y1),l),((x2,y2),u),((x3,y3),p) = lower,upper,pivot in
      let ((x,y),mat') = mult pivot mat in
      for j = 0 to n - 1 do
        l.(j).(j) <- C.one;
        for i = 0 to j do
          let sum = ref C.zero in
            for k = 0 to i - 1 do
              sum := C.add (!sum) (C.multiply u.(k).(j) l.(i).(k))
            done;
          u.(i).(j) <- C.subtract mat'.(i).(j) (!sum)
        done;
        for i = j to n - 1 do
          let sum = ref C.zero in
            for k = 0 to j - 1 do
              sum := C.add (!sum) (C.multiply u.(k).(j) l.(i).(k))
            done;
          let sub = C.subtract mat'.(i).(j) (!sum) in
          l.(i).(j) <- C.divide sub u.(j).(j)
        done;
      done;
      (lower,upper,pivot),s
    else raise ImproperDimensions 

  (* Computes the determinant of a matrix *)
  let determinant (m: matrix) : elt =
    try 
      let ((n,p),m') = m in
      if n = p then 
        let rec triangualar_det (a,mat) curr_index acc =
          if curr_index < n then
            let acc' = C.multiply mat.(curr_index).(curr_index) acc in
            triangualar_det (a,mat) (curr_index + 1) acc' 
          else acc in
        let ((dim1,l),(dim2,u),(dim3,p)),s = lu_decomposition m in
        let det1, det2 = triangualar_det (dim1,l) 0 C.one, 
          triangualar_det (dim2,u) 0 C.one in
        if s mod 2 = 0 then C.multiply det1 det2 
        else C.subtract C.zero (C.multiply det1 det2)
      else raise ImproperDimensions
    with
    | Failure "create_ratio infinite or undefined rational number" -> C.zero


  (*************** Optional module functions ***************)

  (*************** Tests ***************)
  let print_loc i j =  print_string ("Failed @ " ^ string_of_int i ^ " and " ^
            string_of_int j)

  (* Generates a linearly independent matrix *)
  let gen_non_independent x y =
    let mat = empty x y in
    for i = 1 to x-1 do
      for j = 1 to y do
        set_elt mat (i,j) (C.generate_random (float (x + y)) ()) 
      done;
    done;
    let (_,row) = get_row mat 1 in
    let _ = set_row mat x row in 
    mat

  let rec test_empty (times: int) : unit =
    if times = 0 then ()
    else
      let dimx, dimy = Random.int times + 1, Random.int times + 1 in
      let ((x,y),t_mat) = empty dimx dimy in
      assert(x = dimx);
      assert(y = dimy);
      for i = 0 to dimx - 1 do
        for j = 0 to dimy - 1 do
          match C.compare C.zero t_mat.(i).(j) with
          | Equal -> ()
          | _ -> print_loc i j
        done;
      done;
      test_empty (times - 1)

  let rec test_map (times: int) : unit =
    let rec test_from_0 (index: int) (n: elt) (((x,y),m): matrix) : unit =
      if times = index then ()
      else
        let ((x',y'),m') = map (C.add n) ((x,y),m) in
        assert(x' = x);
        assert(y' = y);
        for i = 1 to x do
          for j = 1 to y do
            let elt = C.add n m.(i-1).(j-1) in
            match C.compare elt m'.(i-1).(j-1) with
            | Equal -> ()
            | _ -> print_loc i j
          done;
        done;
        test_from_0 (index + 1) (C.add n C.one) ((x',y'),m') in
    let dimx, dimy = Random.int times + 1, Random.int times + 1 in
    let m = empty dimx dimy in
    test_from_0 0 C.zero m 


  let rec test_get_row (times: int): unit =
    if times = 0 then ()
    else
      let dimx, dimy = Random.int times + 1, Random.int times + 1 in
      let (extra,t_mat) = map (fun _ -> C.generate_random (float times) ()) 
        (empty dimx dimy) in
      for i = 1 to dimx do
        let (dim,row) = get_row (extra, t_mat) i in
        assert(dim = dimy);
        for j = 1 to dimy do
          match C.compare t_mat.(i-1).(j-1) row.(j-1) with
          | Equal -> ()
          | _ -> print_loc i j
        done;
      done;
      test_get_row (times - 1)

  let rec test_get_column (times: int): unit =
    if times = 0 then ()
    else
      let dimx, dimy = Random.int times + 1, Random.int times + 1 in
      let (extra,t_mat) = map (fun _ -> C.generate_random (float times) ()) 
        (empty dimx dimy) in
      for j = 1 to dimy do
        let (dim,column) = get_column (extra, t_mat) j in
        assert(dim = dimx);
        for i = 1 to dimx do
          match C.compare t_mat.(i-1).(j-1) column.(i-1) with
          | Equal -> ()
          | _ -> print_loc i j
        done;
      done;
      test_get_column (times - 1)

  let rec test_get_elt (times: int): unit =
    if times = 0 then ()
    else
      let dimx, dimy = Random.int times + 1, Random.int times + 1 in
      let (extra,t_mat) = map (fun _ -> C.generate_random (float times) ()) 
        (empty dimx dimy) in
      for i = 1 to dimx do
        for j = 1 to dimy do
          let elt = get_elt (extra, t_mat) (i,j) in
          match C.compare elt t_mat.(i-1).(j-1) with
          | Equal -> ()
          | _ -> print_loc i j
        done;
      done;
      test_get_row (times - 1)

  let rec test_set_row (times: int): unit =
    if times = 0 then ()
    else
      let dimx, dimy = Random.int times + 1, Random.int times + 1 in
      let (extra,t_mat) = map (fun _ -> C.generate_random (float times) ()) 
        (empty dimx dimy) in
      let rand_array = Array.map (fun _ -> C.generate_random (float times) ())
        (Array.make dimy C.one) in
      for i = 1 to dimx do
        let _ = set_row (extra,t_mat) i rand_array in
        for j = 1 to dimy do
          match C.compare t_mat.(i-1).(j-1) rand_array.(j-1) with
          | Equal -> ()
          | _ -> print_loc i j
        done;
      done;
      test_set_row (times - 1)


  let rec test_set_column (times: int): unit =
    if times = 0 then ()
    else
      let dimx, dimy = Random.int times + 1, Random.int times + 1 in
      let (extra,t_mat) = map (fun _ -> C.generate_random (float times) ()) 
        (empty dimx dimy) in
      let rand_array = Array.map (fun _ -> C.generate_random (float times) ())
        (Array.make dimx C.one) in
      for j = 1 to dimy do
        let _ = set_column (extra,t_mat) j rand_array in
        for i = 1 to dimx do
          match C.compare t_mat.(i-1).(j-1) rand_array.(i-1) with
          | Equal -> ()
          | _ -> print_loc i j
        done;
      done;
      test_set_row (times - 1)

  let rec test_reduce (times: int) : unit =
    if times = 0 then ()
    else
      let dimx, dimy = Random.int times + 1, Random.int times + 1 in
      let t_mat = map (C.add C.one) (empty dimx dimy) in
      let result = reduce C.add C.zero t_mat in
      let e_result = C.generate_x (float (dimx * dimy)) () in
      assert(e_result = result);
      test_reduce (times - 1)

  let rec test_mult (times: int) : unit =
    if times = 0 then ()
    else
      let dimx = Random.int times + 1 in 
      let identity = create_identity dimx in
      let t_mat = map (fun x -> C.generate_random (float times) ())
        (empty dimx dimx) in
      let result = mult identity t_mat in
      for i = 1 to dimx do
        for j = 1 to dimx do
          assert(get_elt t_mat (i,j) = get_elt result (i,j))
        done;
      done;
      test_mult (times - 1)

  let rec test_add (times: int) : unit =
    if times = 0 then ()
    else 
      let dimx= Random.int times + 1 in 
      let identity = create_identity dimx  in
      let t_mat = map (fun x -> C.generate_random (float times) ()) 
        (empty dimx dimx)  in
      let result = add identity t_mat in
      for i = 1 to dimx do
        for j = 1 to dimx do
          if i <> j then assert(get_elt t_mat (i,j) = get_elt result (i,j))
         else assert((C.add C.one (get_elt t_mat (i,j))) = get_elt result (i,j))
        done;
      done;
      test_add (times - 1)

  let rec test_scale (times: int) : unit =
    if times = 0 then ()
    else 
      let dimx,dimy = Random.int times + 1, Random.int times + 1 in 
      let scalar = C.generate_random (float times) () in
      let t_mat = map (fun x -> C.generate_random (float times) ()) 
        (empty dimx dimy)  in
      let result = scale t_mat scalar in
      for i = 1 to dimx do
        for j = 1 to dimy do
         assert((C.multiply scalar (get_elt t_mat (i,j))) = get_elt result (i,j))
        done;
      done;
      test_add (times - 1)

  let rec test_row_reduce (times: int) : unit =
    let dimx = Random.int times + 2 in
    let independent = gen_non_independent dimx dimx in
    let identity = create_identity dimx in
    let reduced = row_reduce independent in
    try 
      assert (not(reduced = identity))
    with Assert_failure _ -> (print independent;print reduced;print identity)

  let rec test_determinamnt (times: int) : unit =
    let dimx = Random.int times + 2 in
    let independent = gen_non_independent dimx dimx in 
    let det = determinant independent in
    assert(det = C.zero)
      


  (*************** End Test Functions ***************)

  let run_tests times = 
    test_empty times;
    test_map times;
    test_get_row times;
    test_get_column times;
    test_get_elt times;
    test_set_row times;
    test_set_column times;
    test_reduce times; 
    test_add times;
    test_mult times;
    test_scale times;
    test_row_reduce times;
    test_determinamnt times;
    ()

end

(* Creating the Matrix Library module! *)
module EltMatrix = MakeMatrix(Elts)
