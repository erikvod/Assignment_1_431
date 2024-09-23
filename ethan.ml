(*Problem 5*)
(*Author: Ethan*)

let rec binCount list =
  match list with
  | [] -> (0, 0)
  | 0 :: tail ->
    let (zeros, ones) = binCount tail in
    (zeros + 1, ones)
  | 1 :: tail ->
    let (zeros, ones) = binCount tail in
    (zeros, ones + 1)
  | _ :: tail -> binCount tail;;

(*Test for problem 5*)
let () =
let list = [0; 1; 0; 2; 1; 1; 3; 1] in
let (zeros, ones) = binCount list in
Printf.printf "Zeros: %d, Ones: %d\n" zeros ones;;

(*Problem 6*)
(*Author: Ethan*)

let rec makepairs x list =
  match list with
  | [] -> []
  | head :: tail -> (x, head) :: makepairs x tail;;

(*Test for problem 6*)
let () =
  let x = 5 in
  let list = [1; 2; 3; 4] in
  let pairs = makepairs x list in
  List.iter (fun (a, b) -> Printf.printf "(%d, %d) " a b) pairs;
  print_endline "";;

(*Problem 7*)
(*Author: Ethan*)

let rec binomial n k =
  match (n, k) with
  | (_, 0) -> 1
  | (n, k) when n = k -> 1
  | (n, k) -> binomial (n-1) (k-1) + binomial (n-1) k

(*Test for problem 7*)
let () =
  let n = 5 in
  let k = 2 in
  let result = binomial n k in
  Printf.printf "Binomial coefficient (%d, %d) = %d\n" n k result

(*Problem 8*)
(*Author: Ethan*)

let rec dup list =
  match list with
  | [] -> []
  | head :: tail -> head :: head :: dup tail

(*Test for problem 8*)
let () =
  let list = [1; 2; 3; 4] in
  let duplicated_lst = dup list in
  List.iter (fun x -> Printf.printf "%d " x) duplicated_lst;
  print_endline ""

(*Problem 9*)
(*Author: Ethan*)

let rec undup lst =
  match lst with
  | [] -> []
  | [_] -> raise (Failure "bad input")
  | head1 :: head2 :: tail ->
    if head1 = head2 then
      head1 :: undup tail
    else
      raise (Failure "bad input")

(*Test for problem 9*)
let () =
  let valid_list = [1; 1; 2; 2; 3; 3; 4; 4] in
  let invalid_list = [1; 1; 2; 3; 3; 4; 4] in
  let unduplicated_list = undup valid_list in
  List.iter (fun x -> Printf.printf "%d " x) unduplicated_list;
  print_endline "";
  (* This will raise an exception *)
  let _ = undup invalid_list in
  ()

  (*the end*)

