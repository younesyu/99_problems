(* # at 3 [ "a" ; "b"; "c"; "d"; "e" ];;
- : string option = Some "c"
# at 3 [ "a" ];;
- : string option = None *)

let rec at index = function
 | [] -> None
 | hd::tl -> if index = 1 then Some hd else at (index - 1) tl;;