let lexbuf = Lexing.from_channel stdin

let ast = Parser.axiome Lexer.lexer lexbuf

let rec containElt l e =
  match l with
  |[] -> false
  |hd::tl -> if hd = e then true else containElt tl e

let checkStateAndStack ast =
  match ast with
  |(is,ss,st,ist,iss),t -> 
    if (containElt st ist)=true && (containElt ss iss)=true then true else false

let rec checkTransitions t = 
  match t with
  |[] -> true
  |hd::tl -> if (isDeterminist hd tl)=true then checkTransitions tl else false
and
isDeterminist t1 t2 =
  match (t1,t2) with
  |_,[] -> true
  |(x1,x2,x3,x4,x5),(y1,y2,y3,y4,y5)::tl -> if x1=y1 && x2=y2 && x3=y3 then false else isDeterminist t1 tl

let verifyAutomate ast =
  if (checkStateAndStack ast)=true then
    match ast with
    |(d,t) -> if (checkTransitions t)=true then true else false
  else false

let rec update src dst =
  match dst with
  |[] -> src
  |hd::tl -> update (hd::src) tl

let rec getTransition st ss tran letter =
  match tran with
  |[] -> if letter = "" then failwith "L’entrée est épuisée sans que la pile soit vide." else failwith "Il n’y a aucune transition qui s’applique."
  |(l1,l2::lf2,l3,l4,l5)::tl -> 
    if st=l1 then
      if letter=l2 then
        if ss=l3 then (l1,l2,l3,l4,l5)
        else (getTransition st ss tl letter)
      else if l2="" then
        if ss = l3 then (l1,l2,l3,l4,l5)
        else (getTransition st ss tl letter)
      else (getTransition st ss tl letter)
    else (getTransition st ss tl letter)
  |(l1,[],l3,l4,l5)::tl ->
    if st=l1 then
      if ss=l3 then (l1,"",l3,l4,l5)
      else (getTransition st ss tl letter)
    else (getTransition st ss tl letter)

let getFirst word =
  if String.length word = 0 then "" else String.sub word 0 1

let removeFirst word =
  if String.length word = 0 then "" else String.sub word 1 ((String.length word)-1)

let rec isAcceptable st ss tran word =
  match (word,ss) with
  |"",[] -> true
  |_,[] -> failwith "La pile est vide sans que l’entrée soit épuisée."
  |_,hd::tl ->
    let t = getTransition st hd tran (getFirst word) in
    match t with
    |(l1,l2,l3,l4,l5) ->
      if l2="" then isAcceptable l4 (update tl l5) tran word else isAcceptable l4 (update tl l5) tran (removeFirst word)

let test (ast:Ast.automate) word =
  match ast with
  |(is,ss,st,ist,iss),t -> isAcceptable ist [iss] t word

let _ =
  if (verifyAutomate ast)=true then
    Printf.printf "L'automate est correcte.\n------------------------------\nAffichage de l'automate :\n\n%s------------------------------\n" (Ast.toString ast)
  else
    failwith "Mauvais format d'automate."

let _ =
  Printf.printf "Mot à tester : %s\n" (Sys.argv.(1));
  if (test ast (Sys.argv.(1)))=true then
    Printf.printf "Le mot est acceptable.\n"
  else
    Printf.printf "Le mot n'est pas acceptable.\n"