let sum n = 
  match n with
    | n -> if n <= 0 then 0 else (n*(n+1))/2;;

let circle r = 
  match r with
    | r -> if r <= 0.0 then 0.0 else r *. r *. 3.14;;

let concat s = "Hello " ^ s;;

let xor x y = 
  match x with
    | true -> if y then false else true
    | false -> if y then true else false;;

let triangle x y z = 
  match x, y, z with
    | a, b, c -> if (a <= 0 || b <=0 || c <= 0) then false else (a + b > c && a * a + b * b = c * c)|| (b + c > a && b * b + c * c = a * a) || (a + c > b && a * a + c * c = b * b);;

let int_if_then_else b x y = 
  match b with
    | true -> x + y
    | false -> x - y;;

let sum_of_fun_val a b c d e =
  (a*(d*d)+b*d+c) + (a*(e*e)+b*e+c);;

let comp3 a b c d = let f x = (a*x*x)+(b*x)+c in f (f (f d));;

let string2 s = s ^ s;;


let string256 s = (string2 (string2 (string2 (string2 (string2 (string2 (string2 (string2 s))))))));;


