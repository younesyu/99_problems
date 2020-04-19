let rec last_two = function
  | [] -> None
  | [_] -> None
  | fst::snd::[] -> Some (fst, snd)
  | _::_::tl -> last_two tl;;

last_two [ "a" ; "b" ; "c" ; "d" ];;  
last_two [ "a" ];;