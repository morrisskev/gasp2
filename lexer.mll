{
  open Token
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
    | '(' {PARG}
    | ')' {PARD}
    | ',' {COMMA}
    | ';' {SEMICOLON}
    | lettre as c {LETTRE(c)}
    | eof  { EOF }
    | _    { failwith "Mauvaise syntaxe." }