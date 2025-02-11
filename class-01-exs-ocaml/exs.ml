(* exr 1 *)

let hash x = x * x mod 2 ;;

(* exr 2 *)
let greater_two a b = if a > b then a else b ;;
let greater_tri a b c = greater_two (greater_two a b) c ;;

let maior_idade idade1 idade2 idade3 = if (if idade1 > idade2 then idade1 else idade2) > idade3 then (if idade1 > idade2 then idade1 else idade2) else idade3 ;;


(* Printf.printf "%d\n" x;; *)

(* exr 3 *)
let sum_digits num =
  let rec aux_aum_digits cur_num cur_sum =
    if cur_num <= 0 then cur_sum
    else aux_aum_digits (cur_num / 10) (cur_sum + cur_num mod 10) in
    aux_aum_digits num 0;;

(* exr 4 *)


let rec tribo n = 
  if n <= 2 then 0 
  else if n = 3 then 1 
  else tribo (n - 1) + tribo (n - 2) + tribo (n - 3) ;;



(* Escreva uma função que verifica se um número é primo *)

let verify_prime n =
    let rec aux_verify_prime n div div_count =
      if div <= 0 then div_count
      else aux_verify_prime n (div - 1) ((if n mod div = 0 then 1 else 0)  + div_count) in
  if aux_verify_prime n n 0 = 2 then true
  else false ;;
    


(* Escreva uma função que retorna o próximo primo *)

let rec next_prime n =
  if verify_prime (n+1) then n+1
  else next_prime(n+1) ;;


(* Escreva uma função que recebe um prefixo e devolve uma função que adiciona o prefixo a qualquer string *)

let prefix_concat_func prefix = fun str -> prefix ^ str ;;

let q1 = ((prefix_concat_func "TESTE DE MATEMÁTICA") (" Q1: Qual é o maior primo depois de 1000?")) ;;
let q2 = ((prefix_concat_func "TESTE DE MATEMÁTICA") (" Q2: 27 É primo?")) ;;

print_string q1 ;;
print_string q2 ;;


