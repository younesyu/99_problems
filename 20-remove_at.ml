(*
# remove_at 1 ["a";"b";"c";"d"];;
- : string list = ["a"; "c"; "d"]
*)

let remove_at n list =
  let rec aux i left = function
  | [] -> left
  | hd :: tl -> if i = n then left @ tl
                else aux (i+1) (left @ [hd]) tl
  in
  aux 0 [] list