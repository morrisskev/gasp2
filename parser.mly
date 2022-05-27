%{
    open Ast
%}

%token INPUTSYMBOLS STACKSYMBOLS STATES INITIALSTATE INITIALSTACK TRANSITIONS PROGRAM STATE BEGIN END NEXT TOP PUSH POP CHANGE REJECT COLONS PARG PARD COMMA SEMICOLON EOF
%token<string> LETTRE

%start <Ast.automate> axiome
%%

axiome:
    a = automate EOF {a}

automate:
    d = declarations e=entree {d,e}

entree:
    | transitions {t}
	| program {p}


declarations:
    s1 = is s2 = ss s3 = st s4 = ist s5 = iss {s1,s2,s3,s4,s5}

is:
    INPUTSYMBOLS l = separated_list(COMMA,LETTRE) {l}

ss:
    STACKSYMBOLS l = separated_list(COMMA,LETTRE) {l}

st:
    STATES l = separated_list(COMMA,LETTRE) {l}

ist:
    INITIALSTATE s = LETTRE {s}

iss:
    INITIALSTACK s = LETTRE {s}

transitions:
    TRANSITIONS t = list(transition) {t}

transition:
    PARG l1 = LETTRE COMMA l2 = list(LETTRE) COMMA l3 = LETTRE COMMA l4 = LETTRE COMMA l5 = separated_list(SEMICOLON,LETTRE) PARD {l1,l2,l3,l4,l5}

program:
    PROGRAM STATE l=list(block) END {l}

block: 
    | l1=LETTRE COLONS BEGIN NEXT next = list(instructions_1) {l1,CaseNext(next)}
    | l1=LETTRE COLONS BEGIN TOP top = list(instructions_2) {l1,CaseTop(top)}

instructions_1:
    l=LETTRE COLONS c = choice_next {(l,c)}

instructions_2:
    l=LETTRE COLONS c= choice_top {(l,c)}

choice_next: 
    | POP {Pop()}
    | PUSH l=LETTRE {Push(l)}
    | CHANGE l=LETTRE {Change(l)}
    | REJECT {failwith "reject"}
    | BEGIN TOP l=LETTRE COLONS op=choice END {CaseTop([(l,op)])}

choice_top: 
    | POP {Pop()}
    | PUSH l=LETTRE {Push(l)}
    | CHANGE l=LETTRE {Change(l)}
    | REJECT {failwith "reject"}
    | BEGIN NEXT l=LETTRE COLONS op=choice END {CaseNext([(l,op)])}

choice: 
    | POP {Pop()}
    | PUSH l=LETTRE {Push(l)}
    | CHANGE l=LETTRE {Change(l)}
    | REJECT {failwith "reject"}