(*
# rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
- : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
# rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
- : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
*)

let take list n =
  let rec aux acc i = function
  | [] -> List.rev acc
  | hd :: tl -> if i = n then List.rev acc else aux (hd::acc) (i+1) tl
  in
  aux [] 0 list


let drop list n =
  let rec aux i = function
  | [] -> []
  | hd :: tl -> if i = n then hd :: tl else aux (i+1) tl
  in
  aux 0 list

let rotate list n =
  match n with
  | 0 -> list
  | n when n > 0 ->
      let left = take list n in
      let right = drop list n in
      right @ left
  | n when n < 0 ->
      let len = List.length list in
      let left = take list (len + n) in 
      let right = drop list (len + n) in 
      right @ left

(* Using split from split.ml *)
let split list n =
  let rec aux left i = function
  | [] -> (left, [])
  | hd :: tl -> if i = n then (left@[hd], tl) else aux (left@[hd]) (i+1) (tl)
  in
  aux [] 1 list;;

let rotate list n =
  match n with
  | 0 -> list
  | n when n > 0 ->
      let left, right = split list n in
      right @ left
  | n when n < 0 ->
      let len = List.length list in
      let left, right = split list (len + n) in  
      right @ left