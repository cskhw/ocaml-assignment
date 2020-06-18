(* hw3.ml *)

exception NotImplemented

let rec sum n =
  match n with
    | 1 -> 1
    | n -> n + sum (n - 1);;

let rec fac n = 
  match n with
    | 1 -> 1
    | n -> n * fac (n - 1);;

let rec fib n = 
  match n with
    | 0 -> 1
    | 1 -> 1
    | n -> fib (n - 2) + fib (n - 1);;

let rec gcd m n =
  match m, n with
    | 0, a -> a
    | a, b -> gcd (b / a) a;;

let rec max l =
  match l with
    | [] -> 0
    | x::xs -> if x > max xs then x else max xs;;


type tree = Leaf of int | Node of int * tree * tree

let rec sum_tree t = 
  match t with
    | Leaf(a) -> a
    | Node(a, t1, t2) -> a + sum_tree t1 + sum_tree t2;; 

let rec depth t =
  match t with
    | Leaf a -> 0
    | Node(a, t1, t2) -> if depth t1 > depth t2 then 1 + depth t1 else 1 + depth t2;;

let rec bin_search t x = 
  match t with
    | Leaf _ -> false
    | Node(a, t1, t2) -> if x = a then true else (if x > a then bin_search t2 x else bin_search t1 x);; 

type exp =
      INT of int
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | MOD of exp * exp

let rec interp e = 
  match e with
    | INT a -> a
    | ADD(e1, e2) -> interp e1 + interp e2
    | SUB(e1, e2) -> interp e1 - interp e2
    | MUL(e1, e2) -> interp e1 * interp e2
    | DIV(e1, e2) -> interp e1 / interp e2
    | MOD(e1, e2) -> interp e1 mod interp e2;;

type formula =
      True
    | False
    | Neg of formula
    | Or of formula * formula
    | And of formula * formula
    | Imply of formula * formula

let rec eval e =
  match e with
    | True -> true
    | False -> false
    | Neg f1 -> not (eval f1)
    | Or(f1, f2) -> eval f1 || eval f2
    | And(f1, f2) -> eval f1 && eval f2
    | Imply(f1, f2) -> (not (eval f1)) || eval f2;;


