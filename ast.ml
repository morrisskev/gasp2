type transitions = (string * (string list) * string * string * (string list))

type block = 
  | Pop
  | Push of string
  | Change of string 
  | CaseState of (oper list)
  | CaseTop of (oper list)
  | CaseNext of (oper list) 
  | Reject 
and oper = string * block

type automate = 
  |Transitions of (string list * string list * string list * string *string)* transitions list
  |Program of (string list * string list * string list * string *string)* block list

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
    | Push c -> "push "^c
    | Change n -> "change "^n
    | CaseState l -> "begin\n"^(tabs (t+1))^"case state of \n"^(printOperation l (t+2))^tabulation
	  | CaseTop l -> "begin\n"^(tabs (t+1))^"case top of \n"^(printOperation l (t+2))^tabulation
    | CaseNext l -> "begin\n"^(tabs (t+1))^"case next of \n"^(printOperation l (t+2))^tabulation
    | Reject -> "reject"

and printOperation ops t =
	let tabulation = tabs t in
	match ops with
  	| [] -> ""
    | (c, CaseState([(c2,op)])) :: tl -> tabulation ^c^": begin case state of "^c2^": "^(printInstructions op 0)^" end\n"^(printOperation tl t)
    | (c, CaseTop([(c2,op)])) :: tl -> tabulation ^c^": begin case top of "^c2^": "^(printInstructions op 0)^" end\n"^(printOperation tl t)
    | (c, CaseNext([(c2,op)])) :: tl -> tabulation ^c^": begin case next of "^c2^": "^(printInstructions op 0)^" end\n"^(printOperation tl t)
	  | (c, ins) :: tl -> tabulation^c^":"^(printInstructions ins t)^"\n"^(printOperation tl t)

let trans_or_prog a =
	match a with
	| Transitions (d,t) -> "\ntransitions:\n"^(printTransitions t)
	| Program (d,p) -> "\nprogram: "^(printInstructions p 1)

let toString ast =
  match ast with
  |(is,ss,st,ist,iss),t -> "input symbols: "^(print is)^"\nstack symbols: "^(print ss)^"\nstates: "^(print st)^"\ninitial state: "^ist^"\ninitial stack symbol: "^iss^(trans_or_prog t)