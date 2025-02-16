(*  Lists *)

let rec print_list lst =
  match lst with 
  | [] -> 0
  | h :: t -> print_int h ; print_string " "; print_list t ;;

let lista : int list  = [1; 2; 3; 4] ;;

print_list lista ;;
print_endline " ";;

(* Int Function List *)
let rec sum_lst lst = 
  match lst with 
  | [] -> 0
  | h :: t -> h + sum_lst t ;;

print_int (sum_lst lista) ;; 
print_endline " ";;


(* Polymorfic Function *)
let rec lst_lenth lst = 
  match lst with 
  | [] -> 0
  | _ :: t -> 1 + lst_lenth t ;;

(* High Order List Function *)
let square x = x * x ;;

let rec map f u = 
  match u with
  | [] -> []
  | h :: t -> f h :: map f t ;;


let lista_sqr = map square lista;;

print_list lista_sqr ;;
print_endline " ";;
