(*
* hw5.ml
* Feel free to define any helper functions.
* You don't need to define a recursive function if possible.
*)

exception NotImplemented

(**********************************)
(*            Problem 1           *)
(**********************************)
module Problem1 = struct
  open Lambda

  let rec check : exp -> bool =
    fun x -> match x with
      | Lambda(x, Var(y)) -> if x = y then true else false
      | Lambda(x, Lambda(y, Var(z))) -> if x = y && y = z && x = z then true else false
      | Lambda(x, Lambda(y, App (Var z, Var a))) -> if x = z && y = a then true else false
      | Lambda(x, App (Var y, Lambda(z, Var a))) -> if x = y && y = a && x = a then true else false
      | _ -> false

end

(**********************************)
(*            Problem 2           *)
(**********************************)
module Problem2 = struct
  open Kml
  exception Error

  let rec eval : exp -> env -> value =
    fun exp env ->
    match exp with
      | NUM n -> Int n
      | VAR x -> apply_env env x
      | ADD (e1, e2) -> 
          let v1 = eval e1 env in
          let v2 = eval e2 env in
            (match v1, v2 with
              | Int n1, Int n2 -> Int (n1 + n2)
              | _ -> raise Error)
              | SUB (e1, e2) -> 
                  let v1 = eval e1 env in
                  let v2 = eval e2 env in
                    (match v1, v2 with
                      | Int n1, Int n2 -> Int (n1 - n2)
                      | _ -> raise (Failure "Type Error: non-numeric values"))
              | MUL (e1, e2) -> 
                  let v1 = eval e1 env in
                  let v2 = eval e2 env in
                    (match v1, v2 with
                      | Int n1, Int n2 -> Int (n1 * n2)
                      | _ -> raise (Failure "Type Error: non-numeric values"))
              | DIV (e1, e2) -> 
                  let v1 = eval e1 env in
                  let v2 = eval e2 env in
                    (match v1, v2 with
                      | Int n1, Int n2 -> Int (n1 / n2)
                      | _ -> raise (Failure "Type Error: non-numeric values"))
              | UMINUS e -> let v1 = eval e env in 
                    (match v1 with 
                      |Int n1 -> Int (-n1)
                      | _ -> raise (Failure "Type Error: non-numeric values"))
              | EQ (e1, e2) -> 
                  let v1 = eval e1 env in
                  let v2 = eval e2 env in
                    (match v1, v2 with
                      | Int n1, Int n2 -> if n1 = n2 then Bool(true)else Bool(false)
                      | _ -> raise (Failure "Type Error: non-numeric values"))
              | LE (e1, e2) -> 
                  let v1 = eval e1 env in
                  let v2 = eval e2 env in
                    (match v1, v2 with
                      | Int n1, Int n2 -> if n1 <= n2 then Bool(true) else Bool(false)
                      | _ -> raise (Failure "Type Error: non-numeric values"))
              | LT (e1, e2) -> 
                  let v1 = eval e1 env in
                  let v2 = eval e2 env in
                    (match v1, v2 with
                      | Int n1, Int n2 -> if n1 < n2 then Bool(true) else Bool(false)
                      | _ -> raise (Failure "Type Error: non-numeric values"))
              | GE (e1, e2) -> 
                  let v1 = eval e1 env in
                  let v2 = eval e2 env in
                    (match v1, v2 with
                      | Int n1, Int n2 -> if n1 >= n2 then Bool(true) else Bool(false)
                      | _ -> raise (Failure "Type Error: non-numeric values"))
              | GT (e1, e2) -> 
                  let v1 = eval e1 env in
                  let v2 = eval e2 env in
                    (match v1, v2 with
                      | Int n1, Int n2 -> if n1 >= n2 then Bool(true) else Bool(false)
                      | _ -> raise (Failure "Type Error: non-numeric values"))
              | NOT (e) -> 
                  let v = eval e env in
                    (match v with
                      | Bool n -> Bool(not n)
                      | _ -> raise (Failure "Type Error: non-boolean values"))
              | OR (e1, e2) -> 
                  let v1 = eval e1 env in
                  let v2 = eval e2 env in
                    (match v1, v2 with
                      | Bool v1, Bool v2 -> Bool (v1 || v2)
                      | _ -> raise (Failure "Type Error: non-boolean values"))
              | AND (e1, e2) -> 
                  let v1 = eval e1 env in
                  let v2 = eval e2 env in
                    (match v1, v2 with
                      | Bool v1, Bool v2 -> Bool (v1 && v2)
                      | _ -> raise (Failure "Type Error: non-boolean values"))
              | ISZERO e ->
                  (match eval e env with
                    | Int n when n = 0 -> Bool true
                    | _ -> Bool false)
              | IF (e1,e2,e3) ->
                  (match eval e1 env with
                    | Bool true -> eval e2 env
                    | Bool false -> eval e3 env
                    | _ -> raise (Failure "Type Error"))
              | LET (x,e1,e2) ->
                  let v1 = eval e1 env in
                    eval e2 (extend_env (x,v1) env)
              | LETREC(f, x, e1, e2) -> 
                  let closure = RecClosure(f, x, e1, env)
                  in eval e2 (extend_env (f, closure) env)
              | FUN (x, e) -> Closure(x, e, env)
              | APP(e1, e2) -> 
                  let closure = eval e1 env in
                    (match closure with
                      | Closure(x, funBody, funEnv) ->
                          let v2 = eval e2 env
                          in eval funBody (extend_env (x, v2) funEnv)
                      | RecClosure(f, x, funBody, funEnv) -> (* NEW CASE *)
                          let v2 = eval e2 env
                          in eval funBody (extend_env (f,closure) (extend_env (x, v2) funEnv))           
                      | _ -> raise (Failure "Application wants to see a closure!"))

             let rec run : program -> value =
               fun e -> eval e empty_env

end
(***********************************)
(*            Problem 3            *)
(***********************************)

module Problem3 = struct
  open Nameless
  exception CannotTranslate

  let rec findlex x c l=
    match l with
      | [] -> raise (Failure "Non_count_error")
      | h :: t -> if x = h then c else findlex x (c + 1) t

  let addlex x env = x :: env


  let rec eval_nl exp env = 
    match exp with
        NUM x -> NL_NUM x
      | VAR x -> NL_VAR(findlex x 0 env)
      | ADD (e1, e2) -> NL_ADD(eval_nl e1 env, eval_nl e2 env)
      | SUB (e1, e2) -> NL_SUB(eval_nl e1 env, eval_nl e2 env)
      | MUL (e1, e2) -> NL_MUL(eval_nl e1 env, eval_nl e2 env)
      | DIV  (e1, e2) -> NL_DIV(eval_nl e1 env, eval_nl e2 env)
      | UMINUS  (e) -> NL_UMINUS(eval_nl e env)
      | EQ  (e1, e2) -> NL_EQ(eval_nl e1 env, eval_nl e2 env)
      | LE  (e1, e2) -> NL_LE(eval_nl e1 env, eval_nl e2 env)
      | LT  (e1, e2) -> NL_LT(eval_nl e1 env, eval_nl e2 env)
      | GE  (e1, e2) -> NL_GE(eval_nl e1 env, eval_nl e2 env)
      | GT  (e1, e2) -> NL_GT(eval_nl e1 env, eval_nl e2 env)
      | NOT  (e) -> NL_NOT(eval_nl e env)
      | OR  (e1, e2) -> NL_OR(eval_nl e1 env, eval_nl e2 env)
      | AND  (e1, e2) -> NL_AND(eval_nl e1 env, eval_nl e2 env)
      | ISZERO  (e) -> NL_ISZERO(eval_nl e env)
      | IF  (e1, e2, e3) -> NL_IF(eval_nl e1 env, eval_nl e2 env, eval_nl e3 env)
      | LET  (x, e1, e2) -> NL_LET(eval_nl e1 env, eval_nl e2 (addlex x env))
      | FUN  (x, e) -> NL_FUN(eval_nl e (addlex x env))
      | APP  (e1, e2) -> NL_APP(eval_nl e1 env, eval_nl e2 env)



  let rec translate : program -> nl_program =
    fun pgm -> eval_nl pgm []





end

(**********************************)
(*            Problem 4           *)
(**********************************)
module Problem4 = struct
  open Nameless
  exception Error

  let rec ufindlex c env =
    match env with
      | [] -> raise Error
      | h::t -> if c = 0 then h else ufindlex(c-1) t
  let addlex x env = x::env

  let rec eval : nl_exp -> nl_env -> nl_value = 
    fun exp env ->
    match exp with
      |NL_NUM n -> NL_Int n
      |NL_VAR x -> ufindlex x env
      |NL_ADD(e1, e2) -> let v1 = eval e1 env in
          let v2 = eval e2 env in
            (match v1, v2 with
              |NL_Int n1, NL_Int n2 -> NL_Int(n1 + n2)
              | _ -> raise Error)
      |NL_SUB(e1, e2) -> let v1 = eval e1 env in
          let v2 = eval e2 env in
            (match v1, v2 with
              |NL_Int n1, NL_Int n2 -> NL_Int(n1 - n2)
              | _ -> raise Error)
      |NL_MUL(e1, e2) -> let v1 = eval e1 env in
          let v2 = eval e2 env in
            (match v1, v2 with
              |NL_Int n1, NL_Int n2 -> NL_Int(n1 * n2)
              | _ -> raise Error)
      |NL_DIV(e1, e2) -> let v1 = eval e1 env in
          let v2 = eval e2 env in
            (match v1, v2 with
              |NL_Int n1, NL_Int n2 -> NL_Int(n1 / n2)
              | _ -> raise Error)
      |NL_UMINUS e -> 
          let v1 = eval e env in
            (match v1 with 
              |NL_Int n1 -> NL_Int(-n1)
              | _ -> raise Error)
      |NL_EQ(e1, e2) -> 
          let v1 = eval e1 env in
          let v2 = eval e2 env in
            (match v1, v2 with
              | NL_Int n1, NL_Int n2 -> if n1 = n2 then NL_Bool(true)else NL_Bool(false)
              | _ -> raise (Failure "Type Error: non-numeric values"))
      |NL_LE(e1, e2) -> 
          let v1 = eval e1 env in
          let v2 = eval e2 env in
            (match v1, v2 with
              |NL_Int n1, NL_Int n2 -> if n1 <= n2 then NL_Bool(true)else NL_Bool(false)
              | _ -> raise Error)
      |NL_LT(e1, e2) -> 
          let v1 = eval e1 env in
          let v2 = eval e2 env in
            (match v1, v2 with
              |NL_Int n1, NL_Int n2 -> if n1 < n2 then NL_Bool(true)else NL_Bool(false)
              | _ -> raise Error)
      |NL_GE(e1, e2) -> 
          let v1 = eval e1 env in
          let v2 = eval e2 env in
            (match v1, v2 with
              |NL_Int n1, NL_Int n2 -> if n1 >= n2 then NL_Bool(true)else NL_Bool(false)
              | _ -> raise Error)
      |NL_GT(e1, e2) -> 
          let v1 = eval e1 env in
          let v2 = eval e2 env in
            (match v1, v2 with
              |NL_Int n1, NL_Int n2 -> if n1 > n2 then NL_Bool(true)else NL_Bool(false)
              | _ -> raise Error)
      | NL_NOT (e) -> 
          let v1 = eval e env in
            (match v1 with
              | NL_Bool b -> NL_Bool(not b)
              | _ -> raise Error)
      | NL_OR  (e1, e2) -> 
          let v1 = eval e1 env in
          let v2 = eval e2 env in
            (match v1, v2 with
              | NL_Bool b1, NL_Bool b2 -> NL_Bool(b1 || b2)
              | _ -> raise Error)

      | NL_AND  (e1, e2) -> 
          let v1 = eval e1 env in
          let v2 = eval e2 env in
            (match v1, v2 with
              | NL_Bool b1, NL_Bool b2 -> NL_Bool(b1 && b2)
              | _ -> raise Error)
      | NL_IF (e1,e2,e3) -> 
          let v1 = eval e1 env in
            (match v1 with
                NL_Bool true -> eval e2 env
              | NL_Bool false -> eval e3 env
              | _ -> raise Error)
      | NL_ISZERO  (e) ->
          let v1 = eval e env in
            (match v1 with
              |NL_Int b -> if b = 0 then NL_Bool true
                  else NL_Bool false
              |_ -> raise Error)
      | NL_LET  (e1, e2) ->
          let v1 = eval e1 env in
            eval e2 (addlex v1 env)
      | NL_FUN  (e) -> NL_Closure(e, env)
      | NL_APP  (e1, e2) ->
          let v1 = eval e1 env in
          let v2 = eval e2 env in
            (match v1 with
              |NL_Closure(exp,env) -> eval exp (addlex v2 env)
              |_ -> raise Error)




  let rec nl_run : nl_program -> nl_value =
    fun pgm -> eval pgm []

end
