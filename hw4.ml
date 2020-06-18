let rec list_add l1 l2 =
  match l1, l2 with
    | [], l2 -> l2;
    | l1 , [] -> l1;
    | a::x, b::y -> (a + b)::list_add x y;;


let rec insert x l =
  match l with
    | [] -> x::[]
    | a::xs -> if x < a then x::a::xs else a::(insert x xs);;


let rec insort l = 
  match l with
    | [] -> []
    | x :: xs -> insert x (insort xs);;

let rec ltake l n =
  match l, n with
    | [], _ -> []
    | x::xs, m -> x::(ltake xs (m-1))

let rec lall f l =
  match l with
    | [] -> true
    | x :: xs -> if f x then lall f xs else false;;



let rec lmap f l =
  match l with
    | [] -> []
    | x :: xs -> f x :: lmap f xs;;


let rec lfilter f l = 
  match l with
    | [] -> []
    | x :: xs -> if f x then x :: lfilter f xs else lfilter f xs;;


let rec ltabulate n f =
  match n with
    | 0 -> []
    | m -> (f (m - 1)) :: (ltabulate (m - 1) f);;


let lrev l =
  let rec aux acc = function
    | [] -> acc
    |h::t -> aux (h::acc) t in aux [] l ;;


let rec lconcat l = 
  match l with
    | [] -> []
    | x :: xs -> x @ lconcat xs;;


let rec lfoldl f e l =
  match l with
    | [] -> e
    | x::xs -> lfoldl f (f (x, e)) xs;;



let rec lzip l1 l2 = 
  match l1, l2 with
    |l1, [] -> []
    |[], l2 -> []
    | x::xs, y::ys -> (x, y) :: lzip xs ys;;


let rec splita l = 
  match l with
    | [] -> []
    | _ :: [] -> []
    | x::(y::xs) -> x :: splita xs;; 

let rec splitb l = 
  match l with
    | [] -> []
    | _::[] -> []
    | x::y::xs -> y :: splitb xs;;

let split l = 
  (splita l, splitb l);;


let rec cart x l =
  match l with
    |[] -> []
    | h :: t -> (x, h) :: (cart x t);; 


let rec cartprod l1 l2 =
  match l1, l2 with
    | _, []  -> []
    | [], _ -> []
    | x :: xs, y -> (cart x y) @ cartprod xs y;;


























