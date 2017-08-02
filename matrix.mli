
(** A square matrix of floating point numbers. *)
type matrix

(** Creates a square matrix with elements given by the provided function. 
    Specifically [create_square_matrix n f] produces an [n]-by-[n] matrix 
    which has the value [f i j] at position [i,j]. *)
val create_square_matrix : int -> (int -> int -> float) -> matrix

(** Returns the element at the specified position. The first argument is the zero-indexed row number, 
    the second argument is the zero-indexed column number. *)
val get_element : matrix -> int -> int -> float

(** Returns the identity matrix of the specified dimension. *)
val identity_matrix : int -> matrix

(** Inverts a matrix. *)
val invert : matrix -> matrix

(** Multiplies two matrices. *)
val multiply : matrix -> matrix -> matrix

(** Pointwise addition. *)
val add : matrix -> matrix -> matrix

(** Pointwise subtraction. *)
val subtract : matrix -> matrix -> matrix

(** Multiplies a column vector, given in the form of a list, by a matrix. 
    The result is another column vector. *)
val mult_vec_by : float list -> matrix -> float list

(** Multiplies a matrix by a row vector, given in the form of a list.
    The result is another row vector. *)
val mult_by_vec : matrix -> float list -> float list

(** Prints out a matrix. Used for debugging. *)
val print : matrix -> unit

