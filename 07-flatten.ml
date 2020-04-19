(* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)     
type 'a node =
  | One of 'a 
  | Many of 'a node list;;

(**
flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
- : string list = ["a"; "b"; "c"; "d"; "e"]
*)


(* Non-tail-recursive *)
let rec flatten = function
  | [] -> []
  | hd::tl ->
    match hd with
    | One elt -> elt::flatten tl
    | Many elts -> (flatten elts)@flatten tl;;

(* Tail-recursive *)
let flatten list =
  let rec aux acc = function
    | [] -> acc
    | hd::tl ->
      match hd with
      | One elt -> aux (elt::acc) tl
      | Many elts -> aux (aux acc elts) tl
  in
  List.rev (aux [] list);;