{
   open Read 
}

let skip = [' ''\t']|'(''*'[^'\n']*'*'')' (* eat whitespace and ignore comments *)
let letter = ['_''#''$''%''&''\'''*''+'',''-''.''!''/'':'';''\\''^''<''>''A'-'Z''a'-'z''0'-'9'] (* jth added bang for french *)

rule token = parse
   |  ['A'-'Z'] ['_''-''0'-'9']* ('_' letter*)?          { CAT (Lexing.lexeme lexbuf) }   (* I suspect this line is unnecessary now, but not confident enough to delete it *)
   |  ['A'-'Z'] letter*                                  { CAT (Lexing.lexeme lexbuf) }
   |  ['a'-'z'] letter*                                  { CAT (Lexing.lexeme lexbuf) }
   |  '"' skip* ([^'\n'' ''\t''(']+ as term) skip* '"'              { TERM term } (* was letter+, changed to let in ISO8859 chars *)
   |  '"' skip* '"'                                      { TERM_EMPTY }
   |  "-->"                                              { ARROW }
   |  '['                                                { LBRAC }
   |  ']'                                                { RBRAC }
   |  skip+                                              { token lexbuf }
   |  '\n'                                               { NEWLINE }
   |  [ '0'-'9' ]+ as tupleint                           { DIGITS tupleint }
   |  ';'                                                { CONCAT }
   |  ','                                                { COMMA }
   |  [^ '\n']* eof                                      { EOF }
   |  '#' [^ '\n']* eof                                  { EOF }
   |  "/"                                                { SLASH }
   |  eof                                                { EOF }

{

}
