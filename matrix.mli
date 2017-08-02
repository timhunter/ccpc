type matrix

val create_square_matrix : int -> (int -> int -> float) -> matrix

val invert : matrix -> matrix

val multiply : matrix -> matrix -> matrix

val mult_vec_by : float list -> matrix -> matrix

val mult_by_vec : matrix -> float list -> matrix

val print : matrix -> unit

