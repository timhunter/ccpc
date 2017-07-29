open Elts
open Matrix
open MatrixI
open SimplexI

(* Testing the Matrix Library *)
let test times = 
  let _ = EltMatrix.run_tests times in
  let _ = Elts.run_tests times in
  let _ = Simplex.run_tests times in 
  print_string "\nIf no errors above, then all tests passed!\n"

(* Prints a solution *)
let print_solution (e,p) : unit =
  let _ = print_string "Solved!\n\nOptimal value: " in
  let _ = Elts.print e in
  let _ = print_string "\n\nAchieved at the point: " in
  let _ = Simplex.print_point p in
  print_string "\n\n"



(* Parse command-line arguments. Parses filename *)
let parse_args () : unit =
  let usage s = 
    let message = "Possible commands:\n'simplex' {file}\n'run tests' {times}\n" 
    in
    Printf.printf ("usage: %s arguments.\nError-> %s\n") 
     Sys.argv.(0) s; print_string message; exit 1 in
  if Array.length Sys.argv <> 3 then usage "Incorrect number of arguments.";
  match String.lowercase (Sys.argv.(1)) with
  | "run tests" | "run_tests" -> 
    (try 
      let times = int_of_string Sys.argv.(2) in
      test times
    with
      | Failure s -> usage s)
  | "solve"-> 
    (try 
      let filename =  Sys.argv.(2) in
      (match Simplex.load_file filename with
        | None -> (print_string "\nThis system has no feasable solution.\n")
        | Some sys -> 
          let _ = print_string "\nSolving your system....\n\n" in
          match Simplex.solve sys with
          | None -> print_string "This system is unbounded. You can increase/decrease it as you please!\n"
          | Some solution -> print_solution solution)
    with
      | Sys_error e -> usage e) 
  | _ -> usage "Incorrect inputs."
;;

