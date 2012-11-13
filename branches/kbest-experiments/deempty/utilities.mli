val string_of_grammar : Rule.t list -> string

(* Define a "pretty printer" for grammars by wrapping 
 * the string_of_grammar function
 *
 * install using: 
 * #load Rule.cmo
 * #load Utilities.cmo
 * #install_printer Utilities.printer;;
 *)
val printer : Rule.t list -> unit
