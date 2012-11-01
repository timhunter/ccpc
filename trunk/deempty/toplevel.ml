let readmcfg f = Parse.mcfgrule Lexer.token (Lexing.from_channel (open_in f))
