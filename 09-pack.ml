(*# pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]*)

let pack list =
  let rec aux current acc = function
  | [] -> []
  | [elt] -> (elt :: current) :: acc
  | fst ::(snd :: _ as tl) -> 
    if fst = snd then aux (fst :: current) acc tl
    else aux [] ((fst :: current) :: acc) tl
  in
  List.rev (aux [] [] list);;