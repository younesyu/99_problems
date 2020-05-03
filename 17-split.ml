(*
# split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
- : string list * string list =
(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
# split ["a";"b";"c";"d"] 5;;
- : string list * string list = (["a"; "b"; "c"; "d"], [])
*)

let split list n =
  let rec aux left i = function
  | [] -> (left, [])
  | hd :: tl -> if i = n then (left@[hd], tl) else aux (left@[hd]) (i+1) (tl)
  in
  aux [] 1 list;;