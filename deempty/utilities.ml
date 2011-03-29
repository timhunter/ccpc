open Rule

let string_of_grammar (g : Rule.t list) = 
  let rec grammar_to_string ret g =  
    (let rec show_rw ret = 
       (function
       h::t -> show_rw (ret^h^" ") t
	 | [] -> if (compare ret " ") == 0 then "\"\" " else ret) 
     in
     let rec show_sy ret l =
       (let rec show_component ret = 
	  (function
	  Component(i,j)::[] -> (ret^(string_of_int i)^","^(string_of_int j))
	    | Epsilon::[] -> (ret^"Epsilon")
	    | Component(i,j)::t -> show_component (ret^(string_of_int i)^","^(string_of_int j)^";") t
	    | Epsilon::t -> show_component (ret^"Epsilon;") t
	    | _ -> ret) 
	in
	match l with
	    h::t -> show_sy (ret^"["^(show_component "" h)^"]") t
	  | _ -> if (compare ret "[]") == 0 then "" else ret) 
     in
     let show_rule ((t,rw,sy) : Rule.t) =
       t^" --> "^(show_rw "" rw)^(show_sy "" sy)
     in
     match g with
	 h::t -> grammar_to_string (ret^(show_rule h)^"\n") t
       | _ -> ret)
  in
  grammar_to_string "" g
    
(* Define a "pretty printer" for grammars by wrapping 
 * the string_of_grammar function
 *
 * install using: 
 * #load Rule.cmo
 * #load Utilities.cmo
 * #install_printer Utilities.printer;;
 *)
let printer g =
  begin
    Format.open_hvbox 0;
    Format.print_string (string_of_grammar g);
    Format.close_box ()
  end
