type transitions = (string * (string list) * string * string * (string list))

type automate = (string list * string list * string list * string *string)* transitions list

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

let toString ast =
  match ast with
  |(is,ss,st,ist,iss),t -> "input symbols: "^(print is)^"\nstack symbols: "^(print ss)^"\nstates: "^(print st)^"\ninitial state: "^ist^"\ninitial stack symbol: "^iss^"\ntransitions:\n"^(printTransitions t)