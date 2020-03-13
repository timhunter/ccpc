
(** A square matrix of floating point numbers, with a collection of strings acting as the labels 
    of both the rows and the columns; in other words, it's approximately a two-dimensional lookup 
    table indexed by strings. *)
type matrix

(** An exception indicating that we've tried to use a string as an index that does not 
    appear in the relevant matrix's list of indices. *)
exception IndexingError of (string * string list)

(** Creates a square matrix with elements given by the provided function. 
    Specifically [create_square_matrix strs f] produces square matrix with both 
    rows and columns indexed by the list [strs], which has the value [f x y] at 
    the [x]-row in the [y]-column. *)
val create_square_matrix : string list -> (string -> string -> float) -> matrix

(** Returns the element at the specified position. The first [string] argument picks out the row, 
    and the second picks out the column. *)
val get_element : matrix -> string -> string -> float

(** Returns the strings that are valid indices for this matrix. *)
val get_indices : matrix -> string list

(** Returns the identity matrix with rows and columns indexed by the provided list of strings. *)
val identity_matrix : string list -> matrix

(** Inverts a matrix. *)
val invert : matrix -> matrix

(** Multiplies two matrices. *)
val multiply : matrix -> matrix -> matrix

(** Pointwise addition. *)
val add : matrix -> matrix -> matrix

(** Pointwise subtraction. *)
val subtract : matrix -> matrix -> matrix

(** Multiplies a row vector, given in the form of a list, by a matrix. 
    The result is another row vector. *)
val mult_vec_by : float list -> matrix -> float list

(** Multiplies a matrix by a column vector, given in the form of a list.
    The result is another column vector. *)
val mult_by_vec : matrix -> float list -> float list

(** Spectral radius of a matrix. *)
val spectral_radius : matrix -> float

(** Prints out a matrix. Used for debugging. *)
val print : ?ch:(out_channel) -> matrix -> unit

