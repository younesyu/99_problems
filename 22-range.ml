(*
# range 4 9;;
- : int list = [4; 5; 6; 7; 8; 9]
# range 9 4;;
- : int list = [9; 8; 7; 6; 5; 4]
*)


let range i j =
  let rec range_pos i j =
    if i = j then [j]
    else i::(range_pos (i+1) j)
  in
  if i <= j then range_pos i j
  else List.rev (range_pos j i)