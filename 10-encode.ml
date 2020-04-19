(**
# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
*)

let encode list =
  let rec aux count acc = function
  | [] -> []
  | [elt] -> ((count + 1), elt) :: acc
  | fst::(snd::_ as tl) -> 
    if fst = snd then aux (count + 1) acc tl
    else aux 0 ((count + 1, fst) :: acc) tl
  in
  List.rev (aux 0 [] list);;