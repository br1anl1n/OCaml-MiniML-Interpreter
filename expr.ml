(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Divide 
  | Equals
  | LessThan
  | GreaterThan
  | And
  | Or
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *) (* Use intersect to test for the expresssions *)
let rec free_vars (exp : expr) : varidset =
  match exp with 
  | Var v -> SS.singleton v
  | Num _ -> SS.empty 
  | Bool _ -> SS.empty 
  | Unop (_, ex) -> free_vars ex
  | Binop (_, ex1, ex2) -> SS.union (free_vars ex1)(free_vars ex2)
  | Conditional (ex1, ex2, ex3) -> SS.union (SS.union (free_vars ex1)(free_vars ex2))(free_vars ex3)
  | Fun (var, ex) -> SS.remove var (free_vars ex)
  | Let (var, ex1, ex2) -> SS.union (free_vars ex1) (SS.remove var (free_vars ex2))
  | Letrec (var, ex1, ex2) -> SS.union (free_vars ex1) (SS.remove var (free_vars ex2))
  | Raise -> SS.empty
  | Unassigned -> SS.empty
  | App (ex1, ex2) -> SS.union (free_vars ex1) (free_vars ex2)
;;

(* Testing helper function *)
(* let varids_of_list (lst : string list) : varidset =
  List.fold_left (fun set varid -> SS.add varid set) SS.empty lst;; *)
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no other variable names
   use the prefix "var". (Otherwise, they might accidentally be the
   same as a generated variable name.) *)
  

let new_varname () : varid =
  let counter = ref 0 in
  let varname = "var" ^ string_of_int !counter in 
  counter := !counter + 1;
  varname;;
  
  (* let gensym : string -> string =
    let suffix = ref 0 in
    fun str -> let symbol = str ^ string_of_int !suffix in
               suffix := !suffix + 1;
               symbol ;; *)
(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics. 
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  (*var_name is the var_name to be replaced, repl is the expression to be subtituted, and exp is the entire expression*)
  match exp with 
  | Var v -> if v = var_name then repl else exp
  | Num _ -> exp
  | Bool _ -> exp 
  | Unop (unop, ex) -> Unop (unop, subst var_name repl ex)
  | Binop (binop, ex1, ex2) -> Binop (binop, subst var_name repl ex1, subst var_name repl ex2)
  | Conditional (ex1, ex2, ex3) -> Conditional (subst var_name repl ex1, subst var_name repl ex2, subst var_name repl ex3)
  | Fun (var, ex) -> if var = var_name then exp 
                     else if SS.mem var (free_vars repl) then 
                      let fresh_var = new_varname () in 
                      Fun (fresh_var, subst var_name repl (subst var (Var fresh_var) ex))
                     else Fun(var, subst var_name repl ex)

  | Let (var, ex1, ex2) -> if var = var_name then Let(var, subst var_name repl ex1, ex2)
                           else if SS.mem var (free_vars repl) then 
                            let fresh_var = new_varname () in
                            Let (fresh_var, subst var_name repl ex1, subst var_name repl (subst var (Var fresh_var) ex2))
                           else
                            Let (var, subst var_name repl ex1, subst var_name repl ex2)
                             
  | Letrec (var, ex1, ex2) -> if var = var_name then Letrec (var, subst var_name repl ex1, ex2)
                              else if SS.mem var (free_vars repl) then
                                let fresh_var = new_varname () in
                                Letrec (fresh_var, subst var_name repl (subst var (Var fresh_var) ex1), subst var_name repl (subst var (Var fresh_var) ex2))
                              else
                                Letrec (var, subst var_name repl ex1, subst var_name repl ex2)
                                

  | Raise -> exp
  | Unassigned -> exp
  | App (ex1, ex2) -> App (subst var_name repl ex1, subst var_name repl ex2)

;;
     
(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with 
  | Var v -> v
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unop (unop, expr) -> (match unop with 
                          | Negate -> "-" ^ exp_to_concrete_string expr)
                          (* -x *)
  | Binop (binop, ex1, ex2) -> let binop_string = 
                                  (match binop with
                                  | Plus -> "+" 
                                  | Minus -> "-"
                                  | Times -> "*"
                                  | Divide -> "/"
                                  | Equals -> "="
                                  | LessThan -> "<"
                                  | GreaterThan -> ">"
                                  | And -> "&&"
                                  | Or -> "||") 
                              in "(" ^ exp_to_concrete_string ex1 ^ " " 
                                      ^ binop_string 
                                      ^ " " ^ exp_to_concrete_string ex2 ^ ")"
                              (* Example: "(x + 5)" *)

  | Conditional (ex1, ex2, ex3) -> "if " ^ exp_to_concrete_string ex1 ^ 
                                   " then " ^ exp_to_concrete_string ex2 ^
                                   " else " ^ exp_to_concrete_string ex3
  | Fun (var, ex) -> "fun " ^ var ^ " -> " ^ exp_to_concrete_string ex
  | Let (var, ex1, ex2) -> "let " ^ var ^ " = " ^ exp_to_concrete_string ex1 
                                  ^ " in " ^ exp_to_concrete_string ex2
  | Letrec (var, ex1, ex2) -> "let rec" ^ var ^ " = " 
                              ^ exp_to_concrete_string ex1 
                              ^ " in " ^ exp_to_concrete_string ex2
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (ex1, ex2) -> exp_to_concrete_string ex1 ^ " " ^ exp_to_concrete_string ex2
;;


(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var v -> "Var (" ^ v ^ ")"
  | Num n -> "Num (" ^ string_of_int n ^ ")" 
  | Bool b -> "Bool (" ^ string_of_bool b ^ ")"
  | Unop (unop, ex) -> "Unop (" ^ unop_to_string unop ^ ", " ^ exp_to_abstract_string ex ^ ")"
  | Binop (binop, ex1, ex2) -> "Binop (" ^ binop_to_string binop ^ ", " 
                                      ^ exp_to_abstract_string ex1 ^ ", " 
                                      ^ exp_to_abstract_string ex2 ^ ")"

  | Conditional (ex1, ex2, ex3) ->  "Conditional(" ^ exp_to_abstract_string ex1 ^ ", " 
                                                   ^ exp_to_abstract_string ex2 ^ ", " 
                                                  ^ exp_to_abstract_string ex3 ^ ")"

  | Fun (var, ex) -> "Fun(" ^ var ^ ", " ^ exp_to_abstract_string ex ^ ")"
  | Let (var, ex1, ex2) -> "Let(" ^ var ^ ", " 
                                  ^ exp_to_abstract_string ex1 ^ ", " 
                                  ^ exp_to_abstract_string ex2 ^ ")"

  | Letrec (var, ex1, ex2) -> "Letrec(" ^ var ^ ", " ^ exp_to_abstract_string ex1 ^ ", " ^ exp_to_abstract_string ex2 ^ ")"
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (ex1, ex2) -> "App(" ^ exp_to_abstract_string ex1 ^ ", " ^ exp_to_abstract_string ex2 ^ ")"
  and unop_to_string (un : unop) : string = 
    match un with 
    | Negate -> "Negate"

  and binop_to_string (bin : binop) : string = 
    match bin with 
    | Plus -> "Plus"
    | Minus -> "Minus"
    | Times -> "Times"
    | Divide -> "Divide"
    | Equals -> "Equals"
    | LessThan -> "LessThan"
    | GreaterThan -> "GreaterThan"
    | And -> "And"
    | Or -> "Or"
;;
