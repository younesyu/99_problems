type 'a rle =
  | One of 'a
  | Many of int * 'a;;

(*
# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]
*)

let encode list =
  let rle elt = function 
  | 1 -> One elt
  | more -> Many (more, elt)
  in
  let rec aux count acc = function
  | [] -> []
  | [elt] -> (rle elt (count + 1)) :: acc
  | fst::(snd::_ as tl) ->
    if fst = snd then aux (count + 1) acc tl
    else aux 0 ((rle fst (count + 1)) :: acc) tl
  in
  List.rev (aux 0 [] list);;