%{
    open Ast
%}

%token INPUTSYMBOLS STACKSYMBOLS STATES INITIALSTATE INITIALSTACK TRANSITIONS PARG PARD COMMA SEMICOLON EOF
%token<string> LETTRE

%start <Ast.automate> axiome
%%

axiome:
    a = automate EOF{a}

automate:
    d = declarations t = transitions{d,t}

declarations:
    s1 = is s2 = ss s3 = st s4 = ist s5 = iss{s1,s2,s3,s4,s5}

is:
    INPUTSYMBOLS l = separated_list(COMMA,LETTRE){l}

ss:
    STACKSYMBOLS l = separated_list(COMMA,LETTRE){l}

st:
    STATES l = separated_list(COMMA,LETTRE){l}

ist:
    INITIALSTATE s = LETTRE{s}

iss:
    INITIALSTACK s = LETTRE{s}

transitions:
    TRANSITIONS t = list(transition){t}

transition:
    PARG l1 = LETTRE COMMA l2 = list(LETTRE) COMMA l3 = LETTRE COMMA l4 = LETTRE COMMA l5 = separated_list(SEMICOLON,LETTRE) PARD{l1,l2,l3,l4,l5}