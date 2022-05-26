%{
    open Ast
%}

%token INPUTSYMBOLS STACKSYMBOLS STATES INITIALSTATE INITIALSTACK TRANSITIONS PARG PARD COMMA SEMICOLON EOF
%token<char> LETTRE

%start <Ast.automate> axiome
%%

axiome:
    automate EOF{}

automate:
    declarations transitions{}

declarations:
    is ss st ist iss{}

//définir is, ss etc.., transitions..