(* CS431 Assignment 1
Questions 10-13
Author: Stephanie Myalik *)



(* 10. a function that finds the smallest element in a list
(hint: use pattern matching and recursion;
raise (Failure "Empty list") if the list is empty;
there is already a min function that computes the smaller of 2 numbers) *)

let rec find_min lst =
  match lst with
  | [] -> raise (Failure "Empty list")
  | [x] -> x
  | x :: xs ->
      let min_rest = find_min xs in
      min x min_rest

(* Example: smallest = 1 *)
let smallest = find_min [5; 3; 8; 1; 4]

(* 11.
(a) a definition of the type of a binary tree of floats *)

type float_tree =
  | Empty
  | Node of float * float_tree * float_tree

(* (b) the declaration of an actual binary tree of floats, of at least depth (height) 3 *)

let my_tree =
  Node (1.0,    
    Node (2.0,
      Node (4.0, Empty, Empty),
      Node (5.0, Empty, Empty)
    ),
    Node (3.0,
      Empty,
      Node (6.0, Empty, Empty)
    )
  )

(* 12. a function that returns the depth (height) of a binary tree of floats, as defined in 11. *)

let rec tree_depth tree =
  match tree with
  | Empty -> 0                                          
  | Node (_, left, right) ->
      let left_depth = tree_depth left in
      let right_depth = tree_depth right in
      1 + max left_depth right_depth

(* Example: depth = 3 *)
let depth = tree_depth my_tree

(* 13. a function that returns the number of leaf nodes in a binary tree of floats, as defined in 11. *)

let rec count_leaves tree =
  match tree with
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, left, right) ->
      count_leaves left + count_leaves right

(* Example: leaf_count = 3 *)
let leaf_count = count_leaves my_tree