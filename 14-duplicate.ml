(* # duplicate ["a";"b";"c";"c";"d"];;
- : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] *)

let rec duplicate = function
| [] -> []
| hd::tl -> hd::hd::duplicate tl;;