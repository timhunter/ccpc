{
   open Read 
}

rule token = parse
   |  "Epsilon"                                          { EPSILON }
   |  't' ['0'-'9']+ ['a'-'z']* ("_tmp" ['0'-'9'])? ("_eps" | ("_" ['0'-'9']+ "-" ['0'-'9']+))* as cat   {CAT cat}
   |  'S' ("_" ['0'-'9']+ "-" ['0'-'9']+)* as cat                                           { CAT cat}
   |  'E' ("_" ['0'-'9']+ "-" ['0'-'9']+)* ("_eps")?  as cat                                           { CAT cat}
   |  ['_''-''A'-'Z''a'-'z']+                            { TERM (Lexing.lexeme lexbuf)}
   |  "\""                                               { QUOTE }
   |  "-->"                                              { ARROW }
   |  '['                                                { LBRAC }
   |  ']'                                                { RBRAC }
   |  [' ' '\t']                                         { token lexbuf} (* eat whitespace *)
   |  '(' '*'  [^'\n']* '*' ')'                          { token lexbuf} (* ignore comments *)
   |  '\n'                                               { NEWLINE }
   |  [ '0'-'9' ]+ as tupleint                           { INT (int_of_string tupleint) }
   |  ';'                                                { CONCAT }
   |  ','                                                { COMMA }
   |  [^ '\n']* eof                                      { EOF }
   |  '#' [^ '\n']* eof                                  { EOF }
	 |	"/"			                                           { SLASH }
   |  eof                                                { EOF }

{

}
