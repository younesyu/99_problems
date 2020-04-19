let rec last = function
  | [] -> None
  | [elt] -> Some elt
  | _::tl -> last tl;;

last [ "a" ; "b" ; "c" ; "d" ];;
last [];;