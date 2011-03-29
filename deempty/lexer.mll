{
   open Parse
}

rule token = parse   
   |  [^ '\n']* eof                                      { EOF }
   |  '#' [^ '\n']* eof                                  { EOF }
   |  eof                                                { EOF }
   |  't' ['0'-'9']+ ['a'-'z']* as cat                   { CAT cat}
   |  't' ['0'-'9']+ ['a'-'z']* "_tmp" ['0'-'9']+ as cat { CAT cat}
   |  'S'                                                { CAT (Lexing.lexeme lexbuf)}
   |  'E'                                                { CAT (Lexing.lexeme lexbuf)}
   |  ['_''-''A'-'Z''a'-'z']+                            { TERM (Lexing.lexeme lexbuf)}
   |  "\""                                               { QUOTE }
   |  "-->"                                              { ARROW }
   |  '['                                                { LBRAC }
   |  ']'                                                { RBRAC }
   |  [' ' '\t']                                         { token lexbuf } (* eat whitespace *)
   | '(' '*'  [^'\n']* '*' ')'                           { token lexbuf } (* ignore comments *)
   |  '\n'                                               { NEWLINE }
   |  [ '0'-'9' ]+ as tupleint                           { INT (int_of_string tupleint) }
   |  ';'                                                { CONCAT }
   |  ','                                                { COMMA }


{

}
