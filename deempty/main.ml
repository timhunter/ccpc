(* parse.mly is the mcfg file parser and lexer.mll is the lexer *)

let g = ref [];;

let main () = 
  (try
     let lexbuf = Lexing.from_channel stdin in
     g := (Parse.mcfgrule Lexer.token lexbuf)
   with 
       _ -> print_string "Can't parse input mcfg file\n");
  g := Deempty.clean_grammar (Deempty.modify_grammar !g);
  print_string (Utilities.string_of_grammar !g)

let _ = Printexc.print main ()
