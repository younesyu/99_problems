(*
# drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
- : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
*)

let drop list n =
  let rec aux i = function
  | [] -> []
  | hd :: tl -> if i = n then aux 1 tl else hd :: aux (i + 1) tl
  in
  aux 1 list