let rec print_list lst =
  match lst with 
  | [] -> 0
  | h :: t -> print_int h ; print_string " "; print_list t ;;


(* Accesing and element in a List *)

let x = List.nth [1; 2; 3; 4; 10; 23 ;10 ] 5 ;;
print_int x ;; 
print_endline "" ;;

(* Implementation of List.nth *)

let rec get_item lst idx = 
  if idx < 0 then failwith "Negative Index" else
  match lst with 
  | [] -> failwith "Index out of range"
  | h :: t -> if idx = 0 then h else get_item t (idx-1) ;;

(* 
    space O(N)
    time O(N)
*)

let y = get_item [1; 2; 3; 4; 10; 23 ;10 ] 5 ;;
print_int y ;;
print_endline "" ;;


(* list length function *)

(* Faça uma função que retorna a quantidade de elementos em uma lista. Não usar built-in function
List.length *)

let rec lst_length lst = 
  match lst with
  | [] -> 0
  | _ :: t -> 1 + lst_length t ;;

(* let count_elements lst = 
  let a *)
  

(* Biggest *)

(* Faça uma função que retorna o maior elemento de uma lista. *)

let rec lst_biggest lst = 
  match lst with
  | [] -> failwith "List is empty"
  | [x] -> x
  | h :: t -> max h (lst_biggest t) ;;

print_int (lst_biggest [102; 7; 10 ; 15]) ;;
print_endline "" ;;


(* Second biggest *)
(* let rec second_biggest lst = *)
let lst_second_biggest lst =
  let rec aux_second_big lst =  
    match lst with
    | [] | [_] -> failwith "List must have at least lenght 2"
    | [x ; y] -> (max x y, min x y)  
    | h :: t -> 
      let (x, y) = aux_second_big (t) in if h > x then (h, x) else if h > y then (x, h) else (x,y) in

    let (big1, big2)  =  aux_second_big lst in
    big2 ;;

print_int (lst_second_biggest [102; 7; 10 ; 75; 501]) ;;
print_endline "" ;;

(* Faça uma função que recebe uma tupla ponto (x,y) e retorna um texto indicando se x e y são
iguais, x é maior ou y é maior. *)

(* let compare_x_y xy = 
  if x = y then "Equals" else
    if x > y then "X is bigger than Y" else
      "Y is Bigger than X"  ;; *)


  let compare_x_y (x, y) = 
    match x, y with
    | x, y when x = y -> "Equals"
    | x, y when x > y -> "X is bigger then y"
    | _ -> "Y is bigger that X" ;;
  


print_string (compare_x_y (26, 26)) ;;
print_endline "" ;;





(* Fazer uma função que altera todas as ocorrências de uma lista. *)

let rec update_all lst og update = 
  match lst with
  | [] -> []
  | h :: t -> (if h = og then update else h) :: (update_all t og update) ;;

print_list (update_all [2; 5; 3; 2; 90; 2; 10] 2 505) ;;
print_endline "" ;;


(* Record *)

type person = {name: string; age : int; country : string; email: string option} ;;

let tomas = {name = "Tomas"; age = 21; country = "Brazil"; email = Some "tomasalessi@gmail.com"} ;;
let polinna = {name = "Polinna"; age = 17; country = "Brazil"; email = None} ;;


Printf.printf "%s is %d years old from %s\n" tomas.name tomas.age tomas.country;;
Printf.printf "%s is %d years old from %s\n" polinna.name polinna.age polinna.country;;



let is_above_age person_record = 
  if person_record.age > 18 then "Is above age" else
    "Is under age" ;;

print_string (is_above_age tomas) ;;
print_string "\n";;
print_string (is_above_age polinna) ;; 
print_endline "";;

let verify_email person_recod = 
  match person_recod.email with
  | Some email -> "Has email info"
  | None -> "Does not have email info" ;;


print_string (verify_email tomas);;
print_endline "";;
print_string (verify_email polinna);;