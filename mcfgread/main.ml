(* parse.mly is the parser and lexer.mll is the lexer *)

let main () = 
   try
      let lexbuf = Lexing.from_channel stdin in
      let gram = (Parse.mcfgrule Lexer.token lexbuf) in
      (* TODO: Do something with gram...for example, run parser *)
      print_string "File loaded successfully\n"
   with 
      Parsing.Parse_error -> print_string "Can't parse input mcfg file\n"; exit 0
let _ = Printexc.print main ()
