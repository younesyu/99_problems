(*# length [ "a" ; "b" ; "c"];;
- : int = 3
# length [];;
- : int = 0*)

(* Non-tail-recursive version *)
let rec length = function
  | [] -> 0
  | _::tl -> 1 + length tl;;


(* Tail-recursive version *)
let length list =
  let rec aux acc = function
    | [] -> acc
    | _::tl -> aux (acc + 1) tl
  in
  aux 0 list;;