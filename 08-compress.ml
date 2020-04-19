(*
# compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
*)

let rec compress = function
  | fst::(snd::_ as tl) -> 
    if fst = snd then compress tl
    else fst::(compress tl)
  | other_cases -> other_cases;;