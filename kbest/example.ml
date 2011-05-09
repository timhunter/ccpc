let weight = List.fold_left Rational.plusr Rational.zero

let s041 = {arity = 2; tails = [3;4]; head = 1; weight = weight}
let s042 = {arity = 2; tails = [2;10]; head = 1; weight = weight}
let a041 = {arity = 2; tails = [3;4]; head = 2; weight = weight}
let a021 = {arity = 2; tails = [5;6]; head = 3; weight = weight}
let b241 = {arity = 2; tails = [8;9]; head = 4; weight = weight}
let b242 = {arity = 2; tails = [7;10]; head = 4; weight = weight}
let a241 = {arity = 2; tails = [8;9]; head = 7; weight = weight}
let c011 = {arity = 0; tails = []; head = 5; weight = fun l -> (1, 2)}
let d121 = {arity = 0; tails = []; head = 6; weight = fun l -> (1, 2)}
let b441 = {arity = 2; tails = [12;13]; head = 10; weight = weight}
let b442 = {arity = 2; tails = [10;11]; head = 10; weight = weight}
let a441 = {arity = 2; tails = [12;13]; head = 11; weight = weight}
let a442 = {arity = 2; tails = [10;11]; head = 11; weight = weight}
let c441 = {arity = 0; tails = []; head = 12; weight = fun l -> Rational.zero}
let d441 = {arity = 0; tails = []; head = 13; weight = fun l -> Rational.zero}
let c231 = {arity = 0; tails = []; head = 8; weight = fun l -> (1, 2)}
let d341 = {arity = 0; tails = []; head = 9; weight = fun l -> (1, 2)}

let edge_list = [s041; s042; a041; a021; b241; b242; c011; d121; a241; c231; d341; b441; b442; a441; a442; c441; d441]

let edges = List.fold_left (fun es e -> EdgeSet.add e es) EdgeSet.empty edge_list
let vertices = List.fold_left (fun vs v -> VertexSet.add v vs) VertexSet.empty [1;2;3;4;5;6;7;8;9;10;11;12;13]

let hypergraph = {edges = edges; vertices = vertices}

let best_deriv_list = [(1, (s041, [0;0], (2, 1)));
		       (2, (a041, [0;0], (2, 1)));
		       (3, (a021, [0;0], (1, 1)));
		       (4, (b241, [0;0], (1, 1)));
		       (5, (c011, [], (1, 2)));
		       (6, (d121, [], (1, 2)));
		       (7, (a241, [0;0], (1, 1)));
		       (8, (c231, [], (1, 2)));
		       (9, (d341, [], (1, 2)));
		       (10, (b441, [0;0], (0, 1)));
		       (11, (a441, [0;0], (0, 1)));
		       (12, (c441, [], (0, 1)));
		       (13, (d441, [], (0, 1)))]

let best_derivations = List.fold_left (fun bds (v, dw) -> add_to_derivations v dw bds) VertexMap.empty best_deriv_list

let (derivations, cands) = k_best hypergraph best_derivations 1 2
