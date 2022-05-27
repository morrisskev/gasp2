{
  open Parser
}
let space=[' ''\n''\t']
let lettre=['0'-'9''a'-'z''A'-'Z']

rule lexer = parse
    | space {lexer lexbuf}
    | "input symbols:" {INPUTSYMBOLS}
    | "stack symbols:" {STACKSYMBOLS}
    | "states:" {STATES}
    | "initial state:" {INITIALSTATE}
    | "initial stack symbol:" {INITIALSTACK}
    | "transitions:" {TRANSITIONS}
    | "program:" {PROGRAM}
    | "case state of" {STATE}
    | "begin" {BEGIN}
    | "end" {END}
    | "case next of" {NEXT}
    | "case top of" {TOP}
    | "push" {PUSH}
    | "pop" {POP}
    | "change" {CHANGE}
    | "reject" {REJECT}
    | ":" {COLONS}
    | '(' {PARG}
    | ')' {PARD}
    | ',' {COMMA}
    | ';' {SEMICOLON}
    | lettre {LETTRE((Lexing.lexeme lexbuf))}
    | eof  { EOF }
    | _    { failwith "Mauvaise syntaxe." }