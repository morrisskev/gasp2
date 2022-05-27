let lexbuf = Lexing.from_channel stdin

let ast = Parser.axiome Lexer.lexer lexbuf