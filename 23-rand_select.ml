(*
The selected items shall be returned in a list. 
We use the Random module but do not initialize 
it with Random.self_init for reproducibility.
# rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
- : string list = ["g"; "d"; "a"]
*)

open Random

let rand_select list n =
  let rec extract acc n = function
  | [] -> raise Not_found
  | hd :: tl -> if n = 0 then (hd, acc @ tl) 
                else extract (hd :: acc) (n-1) tl
  in
  let extract_rand list len =
    extract [] (Random.int len) list
  in
  let rec aux acc i list len =
    if i = 0 then acc
    else let elt, rest = extract_rand list len in
      aux (elt::acc) (n - 1) rest (len - 1)
  in
  let len = List.length list in
  aux [] (min n len) list len