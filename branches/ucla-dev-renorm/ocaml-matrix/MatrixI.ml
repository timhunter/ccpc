module type MATRIX =
sig

  exception NonSquare
  exception ImproperDimensions

  (******** TYPES ********)
  type elt

  type matrix

  (* empty matrix of nxp dimensions *)
  val empty : int -> int -> matrix

  (* Takes a list of lists and converts that to a matrix *)
  val from_list : (elt list list) -> matrix

  (******** OPERATIONS ON ONE MATRIX ********)
  (* Takes in a matrix and returns its dimensions. ie, nxp *)
  val get_dimensions : matrix -> (int * int)

  (* get's the row of a matrix: Not zero-indexed. *)
  val get_row : matrix -> int -> (int * elt array)

  (* similar to get_row *)
  val get_column: matrix -> int -> (int * elt array)

  (* sets the row of a matrix in place! Not zero-index *)
  val set_row: matrix -> int -> elt array -> unit 

  (* similar to set_row, but for a column *)
  val set_column: matrix -> int -> elt array -> unit

  (* gets the element at the specified index. *)
  val get_elt: matrix -> (int * int) -> elt 

  (* sets the element at the specified index *)
  val set_elt: matrix -> (int * int) -> elt -> unit

  (* Scales every element in the matrix by another elt *)
  val scale : matrix -> elt -> matrix


  (******** MORE ADVANCED SINGLE MATRIX OPERATIONS ********)
  (* Returns the row reduced form of a matrix *)
  val row_reduce: matrix -> matrix
  (* We will implement the algorithm found in the link above *)

  (* Returns the inverse of a matrix *) 
  val inverse: matrix -> matrix

  (*Transposes a matrix. If the input has dimensions m x n, the output will
   * have dimensions n x m *)
  val transpose: matrix -> matrix

  (* Returns the trace of the matrix *)
  val trace: matrix -> elt 
  
  (******** OPERATIONS ON TWO MATRICES ********)
  (* Adds two matrices. They must have the same dimensions *)
  val add : matrix -> matrix -> matrix

  (* Multiplies two matrices. If the matrices have dimensions m x n and p x q, n
   * and p must be equal, and the resulting matrix will have dimension m x q *)
  val mult: matrix -> matrix -> matrix

  (**** Other Library Functions ***)
  (* Function to make over our matrices *)
  val map : (elt -> elt) -> matrix -> matrix

  val iter : (elt -> unit) -> matrix -> unit

  (* Returns the LUP decomposition of a matrix *)
  val lu_decomposition : matrix -> (matrix * matrix * matrix) * int

  (* Returns the determinant of the matrix *) 
  val determinant: matrix -> elt

  (* Takes a string and builds a matrix from it *) 
  val from_string : string -> matrix

  (* Takes a list of lists and converts that to a matrix *)
  val from_list : (elt list list) -> matrix

  (* Prints out the contents of a matrix *)
  val print : matrix -> unit

  (* Runs tests on the Matrix Module *)
  val run_tests : int -> unit

  (************** Other Library Functions *************)
  val iter : (elt -> unit) -> matrix -> unit

  val iteri : (int -> int -> elt -> unit) -> matrix -> unit

  (************** Input/ Output Functions *************)
  (* Generates a matrix from a string. *)
  val load : string -> matrix

  (* Dumps a matrix to a text file whose file name is specified by the string *)
  val dump : string -> matrix -> unit

  (********** Specific for Simplex Algorithm ***********)
  (** All of the following functions will raise ImproperDimensions
   * Exception if the matrix is not the right size for the operation 
   **)

  (* Scales a row *)
  val scale_row: matrix -> int -> elt -> unit

  (* Swaps two rows *)
  val swap_row: matrix -> int -> int -> unit

  (* Subtracts a multiple of one row (the 2nd int) from another (the 1st int) *)
  val sub_mult: matrix -> int -> int -> elt -> unit

end
