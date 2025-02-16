let describe_number_t n = 
  match n with
  | 0 -> "zero"
  | 1 -> "one"
  | 2 -> "two"
  | 3 -> "three"
  | 4 -> "four"
  | 5 -> "five"
  | _ -> "other"  ;;



let describe_number = function
  | n when n = 0 -> "zero"
  | n when n = 1 -> "one"
  | n when n = 2 -> "two"
  | n when n = 3 -> "three"
  | n when n = 4 -> "four"
  | n when n = 5 -> "five"
  | _ -> "other"  ;;



type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat

let int_to_day i = 
  match  i mod 7 with
  | 0 -> Sun
  | 1 -> Mon
  | 2 -> Tue
  | 3 -> Wed
  | 4 -> Thu
  | 5 -> Fri
  | _ -> Sat ;;


let rec sum_list lst = 
  match lst with
  | [] -> 0
  | h :: t -> h + sum_list t ;;


let rec sum_list_t = function
  | [] -> 0
  | h :: t -> h + sum_list_t t ;;

let rec lst_length = function
 | []  -> 0 
 | h :: t -> 1 + lst_length t ;;
 