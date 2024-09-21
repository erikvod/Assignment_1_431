(*Question 1*)
let rec list lst =
  match lst with
  | [] -> 1
  | [ _ ] -> 2
  | _ :: _ :: _ -> 0
;;

let result1 = list [ 1; 2; 3 ]

(*Question 2*)
let rotate lst =
  match lst with
  | [] -> []
  | [ x ] -> [ x ]
  | head :: tail -> tail @ [ head ]
;;

let result2 = rotate [ 1; 2; 3 ]

(*Question 3*)
let remove_last lst =
  match List.rev lst with
  | [] -> []
  | _ :: tail -> List.rev tail
;;

let result3 = remove_last [ 1; 2; 3 ]

(*Question 4*)
let rec remove_element y lst =
  match lst with
  | [] -> []
  | head :: tail ->
    if head = y then remove_element y tail else head :: remove_element y tail
;;

let result4 = remove_element 2 [ 1; 2; 3; 2; 4; 2 ]
