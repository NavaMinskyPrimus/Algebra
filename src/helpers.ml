open Core

let rec getNth mylist index =
  match mylist with
  | [] -> raise (Failure "index doesn't exist")
  | first :: rest -> if index = 0 then first else getNth rest (index - 1)
;;

let failwith msg = raise (Failure msg)

let factors x =
  let rec factorsh x y =
    if x = y
    then [ x ]
    else if x % y = 0
    then y :: factorsh x (y + 1)
    else factorsh x (y + 1)
  in
  factorsh x 1
;;

let gcd x y =
  let rec gcdh x y = if x % y = 0 then y else gcdh y (x % y) in
  gcdh (Int.max x y) (Int.min x y)
;;

let lcm x y = x * y / gcd x y

let gcdList l =
  let rec gcdListh x l =
    match l with
    | [] -> x
    | [ a ] -> gcd x a
    | first :: rest -> gcdListh (gcd x first) rest
  in
  gcdListh 0 l
;;

let rec posExponent a b =
  match b with
  | 0 -> 1
  | _ -> a * posExponent a (b - 1)
;;
