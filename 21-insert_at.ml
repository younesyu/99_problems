(*
  # insert_at "alfa" 1 ["a";"b";"c";"d"];;
- : string list = ["a"; "alfa"; "b"; "c"; "d"]
# insert_at "alfa" 3 ["a";"b";"c";"d"];;
- : string list = ["a"; "b"; "c"; "alfa"; "d"]
# insert_at "alfa" 4 ["a";"b";"c";"d"];;
- : string list = ["a"; "b"; "c"; "d"; "alfa"]
*)

let insert_at elt index list =
  let rec aux i left = function
  | [] -> left @ [elt]
  | (hd :: tl) as right -> if i = index then left @ [elt] @ right
                            else aux (i+1) (left @ [hd]) tl
  in
  aux 0 [] list