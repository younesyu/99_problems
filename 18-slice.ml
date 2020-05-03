(*
# slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
- : string list = ["c"; "d"; "e"; "f"; "g"]
*)

let slice list start finish =
  let rec aux acc i = function
  | [] -> List.rev acc
  | hd :: tl -> if i > finish then List.rev acc
                else if i >= start && i <= finish then
                  aux (hd :: acc) (i+1) tl
                else aux acc (i+1) tl
  in
  aux [] 0 list;;