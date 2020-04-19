(*
# replicate ["a";"b";"c"] 3;;
- : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
*)

let rec replicate list times =
  let rec replicate_item i acc = function
  | 0 -> acc
  | more -> replicate_item i (i::acc) (more - 1)
  in
  match list with
  | [] -> []
  | hd::tl -> (replicate_item hd [] times)@(replicate tl times);;