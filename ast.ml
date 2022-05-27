type transitions = (string * (string list) * string * string * (string list))

type automate = (string list * string list * string list * string *string)* transitions list

type block = 
  | Pop
  | Push of char
  | Change of char 
  | CaseState of (oper list)
  | CaseTop of (oper list)
  | CaseNext of (oper list) 
  | Reject 
and oper = char * block

type entree = 
| Transitions of transitions list
| Program of block


let rec print l =
  match l with
  |[] -> ""
  |hd::tl -> if List.length(tl)>0 then hd ^", "^ (print tl) else hd ^ (print tl)

let rec printTran l =
  match l with
  |[] -> ""
  |hd::tl -> if List.length(tl)>0 then hd ^";"^ (print tl) else hd ^ (print tl)

let rec printTransitions (t: transitions list) =
  match t with
  |[] -> ""
  |(l1,l2,l3,l4,l5)::tl -> "("^l1^","^(print l2)^","^l3^","^l4^","^(printTran l5)^")\n"^(printTransitions tl)

let rec tabs n=
	if(n == 0) then "" 
  else "  " ^ (tabs (n-1))

let rec printInstructions ins t=
	let tabulation = tabs t in
    match ins with
    | Pop -> "pop"
    | Push c -> "push "^Char.escaped c
    | Change n -> "change "^Char.escaped n
    | CaseState l -> "begin\n"^(tabs (t+1))^"case state of \n"^(printOperation l (t+2))^tabulation
	  | CaseTop l -> "begin\n"^(tabs (t+1))^"case top of \n"^(printOperation l (t+2))^tabulation
    | CaseNext l -> "begin\n"^(tabs (t+1))^"case next of \n"^(printOperation l (t+2))^tabulation
    | Reject -> "reject"

and printOperation ops t =
	let tabulation = tabs t in
	match ops with
  	| [] -> ""
    | (c, CaseState([(c2,op)])) :: tl -> tabulation ^Char.escaped c^": begin case state of "^Char.escaped c2^": "^(printInstructions op 0)^" end\n"^(printOperation tl t)
    | (c, CaseTop([(c2,op)])) :: tl -> tabulation ^Char.escaped c^": begin case top of "^Char.escaped c2^": "^(printInstructions op 0)^" end\n"^(printOperation tl t)
    | (c, CaseNext([(c2,op)])) :: tl -> tabulation ^Char.escaped c^": begin case next of "^Char.escaped c2^": "^(printInstructions op 0)^" end\n"^(printOperation tl t)
	  | (c, ins) :: tl -> tabulation^Char.escaped c^":"^(printInstructions ins t)^"\n"^(printOperation tl t)

let trans_or_prog a =
	match a with
	| Transitions t -> "\ntransitions:\n"^(printTransitions t)
	| Program p -> "\nprogram: "^(printInstructions p 1)

let toString ast =
  match ast with
  |(is,ss,st,ist,iss),t -> "input symbols: "^(print is)^"\nstack symbols: "^(print ss)^"\nstates: "^(print st)^"\ninitial state: "^ist^"\ninitial stack symbol: "^iss^(trans_or_prog t)