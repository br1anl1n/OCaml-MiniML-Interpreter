(* 
                         CS 51 Final Project 
                               Testing
 *)

open CS51Utils ;; 
open Absbook ;;
open Expr ;;
open Evaluation ;;
open Env;;

let test () =


  (* exp_to_abstract_string testing  *)
  unit_test (exp_to_abstract_string(Var "x") = 
  "Var (x)") "abstract Var";
  unit_test (exp_to_abstract_string(Num 3) = 
    "Num (3)") "abstract Num";
  unit_test (exp_to_abstract_string(Bool true) = 
    "Bool (true)") "abstract Bool";
  unit_test (exp_to_abstract_string(Unop (Negate, Var "x")) = 
    "Unop (Negate, Var (x))") "abstract Unop";
 
  unit_test (exp_to_abstract_string(Binop (Plus, Num 3, Num 4)) = 
    "Binop (Plus, Num (3), Num (4))") "abstract Binop Plus";
  unit_test (exp_to_abstract_string(Binop (Minus, Num 3, Num 4)) = 
    "Binop (Minus, Num (3), Num (4))") "abstract Binop Minus";
  unit_test (exp_to_abstract_string(Binop (Times, Num 3, Num 4)) = 
   "Binop (Times, Num (3), Num (4))") "abstract Binop Times";
  unit_test (exp_to_abstract_string(Binop (Divide, Num 3, Num 4)) = 
   "Binop (Divide, Num (3), Num (4))") "abstract Binop Divide";
  unit_test (exp_to_abstract_string(Binop (Equals, Num 3, Num 4)) = 
    "Binop (Equals, Num (3), Num (4))") "abstract Binop Equals";
  unit_test (exp_to_abstract_string(Binop (LessThan, Num 3, Num 4)) = 
    "Binop (LessThan, Num (3), Num (4))") "abstract Binop LessThan";
  unit_test (exp_to_abstract_string(Binop (GreaterThan, Num 4, Num 3)) = 
    "Binop (GreaterThan, Num (4), Num (3))") "abstract Binop GreaterThan";
  unit_test (exp_to_abstract_string(Binop (And, Bool true, Bool false)) = 
    "Binop (And, Bool (true), Bool (false))") "abstract Binop And";
  unit_test (exp_to_abstract_string(Binop (Or, Bool true, Bool false)) = 
    "Binop (Or, Bool (true), Bool (false))") "abstract Binop Or";
  unit_test (exp_to_abstract_string(Fun ("x", Binop(Plus, Num 3, Num 4))) = 
    "Fun(x, Binop (Plus, Num (3), Num (4)))") "abstract Fun";
  unit_test (exp_to_abstract_string(Conditional (Var "x", Var "y", Var "z")) = 
    "Conditional(Var (x), Var (y), Var (z))") "abstract Conditonal";

  unit_test (exp_to_abstract_string(Let("f", Fun("x", Var("x")), 
  App(App(Var("f"), Var("f")), Num(3)))) = 
    "Let(f, Fun(x, Var (x)), App(App(Var (f), Var (f)), Num (3)))") "abstract Let";
  
  unit_test (exp_to_abstract_string(Letrec("f", Fun("x", 
  Conditional(Binop(Equals, Var("x"), Num(0)), Num(1),
  Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1)))))),
  App(Var("f"), Num(4)))) = 
    "Letrec(f, Fun(x, Conditional(Binop (Equals, Var (x), Num (0)), Num (1), Binop (Times, Var (x), App(Var (f), Binop (Minus, Var (x), Num (1)))))), App(Var (f), Num (4)))") 
    "abstract Letrec"; 

  unit_test (exp_to_abstract_string(Raise) = 
    "Raise") "abstract raise";
  unit_test (exp_to_abstract_string(Unassigned) = 
    "Unassigned") "abstract unassigned";
  unit_test (exp_to_abstract_string(App(Num(3), Num(4))) = 
    "App(Num (3), Num (4))") "abstract App";

  (* exp_to_concrete_string testing *)
  unit_test (exp_to_concrete_string(Var "x") = 
    "x") "concrete Var";
  unit_test (exp_to_concrete_string(Num 3) = 
    "3") "concrete Num";
  unit_test (exp_to_concrete_string(Bool true) = 
    "true") "concrete Bool";
  unit_test (exp_to_concrete_string(Unop (Negate, Var "x")) = 
    "-x") "concrete Unop";
 
  unit_test (exp_to_concrete_string(Binop (Plus, Num 3, Num 4)) = 
    "(3 + 4)") "concrete Binop Plus";
  unit_test (exp_to_concrete_string(Binop (Minus, Num 3, Num 4)) = 
    "(3 - 4)") "concrete Binop Minus";
  unit_test (exp_to_concrete_string(Binop (Times, Num 3, Num 4)) = 
    "(3 * 4)") "concrete Binop Times";
  unit_test (exp_to_concrete_string(Binop (Divide, Num 15, Num 3)) = 
  "(15 / 3)") "concrete Binop Divide";
  unit_test (exp_to_concrete_string(Binop (Equals, Num 3, Num 4)) = 
    "(3 = 4)") "concrete Binop Equals";
  unit_test (exp_to_concrete_string(Binop (LessThan, Num 3, Num 4)) = 
    "(3 < 4)") "concrete Binop LessThan";
  unit_test (exp_to_concrete_string(Binop (GreaterThan, Num 4, Num 3)) = 
    "(4 > 3)") "concrete Binop GreaterThan";
  unit_test (exp_to_concrete_string(Binop (And, Bool true, Bool false)) = 
    "(true && false)") "concrete Binop And";
  unit_test (exp_to_concrete_string(Binop (Or, Bool true, Bool false)) = 
   "(true || false)") "concrete Binop Or";

  unit_test (exp_to_concrete_string(Conditional (Var "x", Var "y", Var "z")) 
  = "if x then y else z") "concrete Conditonal";
  unit_test (exp_to_concrete_string(Fun ("x", Binop(Plus, Num 3, Num 4))) = 
    "fun x -> (3 + 4)") "concrete Fun";

  unit_test (exp_to_concrete_string(Let("f", Fun("x", Var("x")), 
  App(App(Var("f"), Var("f")), Num(3)))) = 
    "let f = fun x -> x in f f 3") "concrete Let";
  
  unit_test (exp_to_concrete_string(Letrec("f", Fun("x", 
  Conditional(Binop(Equals, Var("x"), Num(0)), Num(1),
  Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1)))))),
  App(Var("f"), Num(4)))) = 
    "let recf = fun x -> if (x = 0) then 1 else (x * f (x - 1)) in f 4") 
    "concrete Letrec";

  unit_test (exp_to_concrete_string(Raise) = 
    "Raise") "concrete raise";

  unit_test (exp_to_concrete_string(Unassigned) = 
    "Unassigned") "concrete unassigned";

  unit_test (exp_to_concrete_string(App(Num(3), Num(4))) = 
    "3 4") "concrete App";


  (* Free Vars testing *)
  unit_test (same_vars (free_vars (Var "x")) (vars_of_list ["x"]))
  "free_vars Var";
  unit_test (same_vars (free_vars (Num 1)) (vars_of_list []))
    "free_vars Num";
  unit_test (same_vars (free_vars (Bool true)) (vars_of_list []))
    "free_vars Bool";
  unit_test (same_vars (free_vars (Unop (Negate, Var "x"))) (vars_of_list ["x"]))
    "free_vars Unop";
  unit_test (free_vars (Binop (Plus, Var "x", Var "y")) = vars_of_list ["x"; "y"]) 
    "free_vars Binop";
  unit_test (free_vars (Conditional (Var "x", Var "y", Var "z")) = vars_of_list ["x"; "y"; "z"]) 
    "free_vars Conditional";
  unit_test (same_vars (free_vars (Fun ("x", Var "y"))) (vars_of_list ["y"]))
    "free_vars Fun";
  unit_test (free_vars (Let ("x", Var "y", Var "z")) = vars_of_list ["y"; "z"]) 
    "free_vars Let";
  unit_test (free_vars (Letrec ("x", Var "y", Var "z")) = vars_of_list ["y"; "z"]) 
    "free_vars Let";
  unit_test (same_vars (free_vars Raise) (vars_of_list []))
    "free_vars Raise";
  unit_test (same_vars (free_vars Unassigned) (vars_of_list []))
    "free_vars Unassigned";
  unit_test (free_vars (App (Var "x", Var "y")) = vars_of_list ["x"; "y"]) 
    "free_vars app";


  (* subst testing *)
  unit_test (subst "x" (Num 2) (Var "y") = 
   Var "y") "subst Var";
  unit_test (subst "x" (Num 1) (Num 2) = 
    Num 2) "subst Num";
  unit_test (subst "x" (Bool false) (Bool true) = 
    Bool true) "subst Bool";
  unit_test (subst "x" (Var "x") (Unop (Negate, Var "x")) = 
    Unop (Negate, Var "x")) "subst Unop";
  unit_test (subst "x" (Num 3) (Binop (Plus, Var "x", Var "y")) = 
    Binop (Plus, Num 3, Var "y")) "subst Binop Plus";
  unit_test (subst "x" (Unop (Negate, Var "x"))  (Binop (Minus, Var "x", Var "y")) = 
    Binop (Minus, Unop (Negate, Var "x"), Var "y")) "subst Binop Minus";
  unit_test (subst "x" (Unop (Negate, Var "x"))  (Binop (Times, Var "x", Var "y")) = 
    Binop (Times, Unop (Negate, Var "x"), Var "y")) "subst Binop Times";
  unit_test (subst "x" (Unop (Negate, Var "x"))  (Binop (Divide, Var "x", Var "y")) = 
    Binop (Divide, Unop (Negate, Var "x"), Var "y")) "subst Binop Divide";
  unit_test (subst "x" (Unop (Negate, Var "x"))  (Binop (Equals, Var "x", Var "y")) = 
    Binop (Equals, Unop (Negate, Var "x"), Var "y")) "subst Binop Equals";
  unit_test (subst "x" (Unop (Negate, Var "x"))  (Binop (LessThan, Var "x", Var "y")) = 
    Binop (LessThan, Unop (Negate, Var "x"), Var "y")) "subst Binop LessThan";
  unit_test (subst "x" (Unop (Negate, Var "x"))  (Binop (GreaterThan, Var "x", Var "y")) = 
    Binop (GreaterThan, Unop (Negate, Var "x"), Var "y")) "subst Binop GreaterThan";
  unit_test (subst "x" (Unop (Negate, Var "x"))  (Binop (And, Bool true, Bool false)) = 
    Binop (And, Bool true, Bool false)) "subst Binop And";
  unit_test (subst "x" (Unop (Negate, Var "x"))  (Binop (Or, Bool true, Bool false)) = 
    Binop (Or, Bool true, Bool false)) "subst Binop Or";

  unit_test (subst "x" (Num 2) (Conditional (Var "x", Var "y", Var "z")) = 
    Conditional (Num 2, Var "y", Var "z")) "subst Conditional";
  unit_test (subst "x" (Num 2) (Fun ("x", Var "y")) = 
    Fun ("x", Var "y")) "subst Fun no substitution";
  unit_test (subst "x" (Num 2) (Fun ("y", Var "x")) = 
    Fun ("y", Num 2)) "subst Fun";
  unit_test (subst "x" (Num 2) (Let ("x", Var "y", Var "z")) = 
    Let ("x", Var "y", Var "z")) "subst Let no substitution";
  unit_test (subst "x" (Num 2) (Let ("y", Var "x", Var "z")) = 
    Let ("y", Num 2, Var "z")) "subst Let";
  unit_test (subst "x" (Num 2) (Letrec ("x", Var "y", Var "z")) = 
    Letrec ("x", Var "y", Var "z")) "subst Letrec no substitution";
  unit_test (subst "x" (Num 2) (Letrec ("y", Var "x", Var "z")) = 
    Letrec ("y", Num 2, Var "z")) "subst Letrec";
  unit_test (subst "x" (Num 2) Raise = 
    Raise) "subst Raise";
  unit_test (subst "x" (Num 2) Unassigned = 
    Unassigned) "subst Unassigned";
  unit_test (subst "x" (Num 2) (App (Var "f", Var "x")) = 
    App (Var "f", Num 2)) "subst App";

  (* env module tests *)
  unit_test (close (Var "x") (Env.empty()) = Closure (Var "x", Env.empty()))
    "Env close empty";
  unit_test (Env.lookup (Env.extend (Env.empty ()) "x" (ref (Env. Val (Num 3)))) "x" = Env.Val (Num 3))
    "Env lookup";
  unit_test (value_to_string (lookup (Env.extend (Env.empty ()) "x" (ref (Env.Val (Num 3)))) "x") = "3")
    "Env value_to_string";
  unit_test (Env.env_to_string (Env.extend (Env.empty ()) "x" (ref (Env. Val (Num 3)))) = "{x->3}{}")
    "Env env_to_string";


  (* eval_s testing*)
  unit_test (try eval_s (Var "x") (Env.empty ()) = Env.Val (Var "test_var") with
            | EvalError "Unbound variable" -> true
            | _ -> false)
    "eval_s var unbound";
  unit_test ((eval_s (Num (1)) (Env.empty ())) = (Env.Val (Num 1)))
    "eval_s num";
  unit_test ((eval_s (Bool true) (Env.empty ())) = (Env.Val (Bool true)))
    "eval_s Bool";
  unit_test ((eval_s (Unop (Negate, Num 5)) (Env.empty ())) = (Env.Val (Num (-5))))
    "eval_s Unop"; 
    

  unit_test ((eval_s (Binop (Plus, (Num 3), (Num 5))) (Env.empty ())) = Env.Val (Num 8))
    "eval_s Binop Plus";
  unit_test ((eval_s (Binop (Minus, (Num 3), (Num 5))) (Env.empty ())) = Env.Val (Num (-2)))
    "eval_s Binop Minus";
  unit_test ((eval_s (Binop (Times, (Num 3), (Num 5))) (Env.empty ())) = Env.Val (Num 15))
    "eval_s Binop Times";
  unit_test ((eval_s (Binop (Divide, (Num 15), (Num 3))) (Env.empty ())) = Env.Val (Num 5))
    "eval_s Binop Times";
  unit_test ((eval_s (Binop (Equals, (Num 3), (Num 3))) (Env.empty ())) = Env.Val (Bool true))
    "eval_s Binop is Equal";
  unit_test ((eval_s (Binop (Equals, (Num 3), (Num 5))) (Env.empty ())) = Env.Val (Bool false))
    "eval_s Binop not Equal";
  unit_test ((eval_s (Binop (LessThan, (Num 3), (Num 5))) (Env.empty ())) = Env.Val (Bool true))
    "eval_s Binop is LessThan";
  unit_test ((eval_s (Binop (GreaterThan, (Num 5), (Num 3))) (Env.empty ())) = Env.Val (Bool true))
    "eval_s Binop is GreaterThan";
  unit_test ((eval_s (Binop (And, Bool true, Bool false)) (Env.empty ())) = Env.Val (Bool false))
    "eval_s Binop And";
  unit_test ((eval_s (Binop (Or, Bool true, Bool false)) (Env.empty ())) = Env.Val (Bool true))
    "eval_s Binop And";

  unit_test ((eval_s (Conditional (Bool true, Num (3), Num (5))) (Env.empty ())) = Env.Val (Num 3))
    "eval_s conditional if true";
  unit_test ((eval_s (Conditional (Bool false, Num (3), Num (5))) (Env.empty ())) = Env.Val (Num 5))
    "eval_s conditional if false";
  unit_test ((eval_s (Conditional (Bool true, Num 10, Num 20)) (Env.extend (Env.empty ()) "x" (ref (Env.Val (Num 5))))) = (Env.Val (Num 10))) 
    "eval_s Conditional and Num";

  unit_test ((eval_s (Let ("x", Num (3), Var "x")) (Env.empty ())) = Env.Val (Num 3))
    "eval_s let x = 3 in x";
  unit_test ((eval_s (Let ("x", Num (3), Binop (Plus, Var "x", Num 5))) (Env.empty ())) = Env.Val (Num 8))
    "eval_s let x = 3 in x + 5";

  unit_test ((eval_s (Letrec ("x", Num (3), Var "x")) (Env.empty ())) = Env.Val (Num 3))
    "eval_s let rec x = 3 in x";
  unit_test ((eval_s (Letrec ("x", Num (3), Binop (Plus, Var "x", Num 5))) (Env.empty ())) = Env.Val (Num 8))
    "eval_s let x = 3 in x + 5";

  unit_test (try (let _ = eval_s (Raise)(Env.empty()) in false) with 
    | EvalException -> true
    | _ -> false )
    "eval_s Raise";
  unit_test (try (let _ = eval_s (Unassigned)(Env.empty()) in false) with 
    | EvalError _ -> true
    | _ -> false )
    "eval_s Unassigned";
  unit_test (eval_s (App (Fun ("x", Num 3), Num 2)) (Env.empty()) = Env.Val (Num 3)) 
    "eval_s App";

  (* eval_d *)
  (* unit_test (eval_d (Var "x") (Env.extend (Env.empty ()) "x" (ref (Env.Val (Num 5))))) = (Env.Val (Num 5))
    "eval_d Var and Num"; *)
  unit_test ((eval_d (Num (1)) (Env.empty ())) = (Env.Val (Num 1)))
    "eval_d num";
  unit_test ((eval_d (Bool true) (Env.empty ())) = (Env.Val (Bool true)))
    "eval_d Bool";
  unit_test ((eval_d (Unop (Negate, Num 5)) (Env.empty ())) = (Env.Val (Num (-5))))
    "eval_d Unop"; 
  unit_test ((eval_d (Unop (Negate, Var "x")) (Env.extend (Env.empty ()) "x" (ref (Env.Val (Num 5))))) = (Env.Val (Num (-5)))) 
    "eval_d Unop and Num";

  unit_test ((eval_d (Binop (Plus, (Num 3), (Num 5))) (Env.empty ())) = Env.Val (Num 8))
    "eval_d Binop Plus";
  unit_test ((eval_d (Binop (Plus, Num 3, Var "x")) (Env.extend (Env.empty ()) "x" (ref (Env.Val (Num 5))))) = (Env.Val (Num 8))) 
    "eval_d Binop and Num"; 
  unit_test ((eval_d (Binop (Minus, (Num 3), (Num 5))) (Env.empty ())) = Env.Val (Num (-2)))
    "eval_d Binop Minus";
  unit_test ((eval_d (Binop (Times, (Num 3), (Num 5))) (Env.empty ())) = Env.Val (Num 15))
    "eval_d Binop Times";
  unit_test ((eval_d (Binop (Divide, (Num 15), (Num 3))) (Env.empty ())) = Env.Val (Num 5))
    "eval_d Binop Times";
  unit_test ((eval_d (Binop (Equals, (Num 3), (Num 3))) (Env.empty ())) = Env.Val (Bool true))
    "eval_d Binop is Equal";
  unit_test ((eval_d (Binop (Equals, (Num 3), (Num 5))) (Env.empty ())) = Env.Val (Bool false))
    "eval_d Binop not Equal";
  unit_test ((eval_d (Binop (LessThan, (Num 3), (Num 5))) (Env.empty ())) = Env.Val (Bool true))
    "eval_d Binop is LessThan";
  unit_test ((eval_d (Binop (LessThan, (Num 5), (Num 3))) (Env.empty ())) = Env.Val (Bool false))
    "eval_d Binop not LessThan";
  unit_test ((eval_d (Binop (GreaterThan, (Num 5), (Num 3))) (Env.empty ())) = Env.Val (Bool true))
    "eval_d Binop is GreaterThan";
  unit_test ((eval_d (Binop (And, Bool true, Bool false)) (Env.empty ())) = Env.Val (Bool false))
    "eval_d Binop And";
  unit_test ((eval_d (Binop (Or, Bool true, Bool false)) (Env.empty ())) = Env.Val (Bool true))
    "eval_d Binop And";


  unit_test ((eval_d (Conditional (Bool true, Num (3), Num (5))) (Env.empty ())) = Env.Val (Num 3))
    "eval_d conditional if true";
  unit_test ((eval_d (Conditional (Bool false, Num (3), Num (5))) (Env.empty ())) = Env.Val (Num 5))
    "eval_d conditional if false";

  unit_test ((eval_d (Let ("x", Num (3), Var "x")) (Env.empty ())) = Env.Val (Num 3))
    "eval_d let x = 3 in x";
  unit_test ((eval_d (Let ("x", Num (3), Binop (Plus, Var "x", Num 5))) (Env.empty ())) = Env.Val (Num 8))
    "eval_d let x = 3 in x + 5";

  unit_test ((eval_d (Letrec ("x", Num (3), Var "x")) (Env.empty ())) = Env.Val (Num 3))
    "eval_d let rec x = 3 in x";
  unit_test ((eval_d (Letrec ("x", Num (3), Binop (Plus, Var "x", Num 5))) (Env.empty ())) = Env.Val (Num 8))
    "eval_d let x = 3 in x + 5";
  unit_test (try (let _ = eval_d (Raise)(Env.empty()) in false) with 
    | EvalException -> true
    | _ -> false )
    "eval_d Raise";
  unit_test (try (let _ = eval_d (Unassigned)(Env.empty()) in false) with 
    | EvalError _ -> true
    | _ -> false)
    "eval_d Unassigned";
  unit_test (eval_d (App (Fun ("x", Num 3), Num 2)) (Env.empty()) = Env.Val (Num 3)) 
    "eval_d App";

  (* unit_test ((eval_s (Unop (Negate, Var "x")) (Env.extend (Env.empty ()) "x" (ref (Env.Val (Num 5))))) = (Exception: IllFormed "can't negate non-integers"))
  "eval_d Unop and Num"; *)

  (* unit_test ((eval_d (Binop (Plus, Num 3, Var "x")) (Env.extend (Env.empty ()) "x" (ref (Env.Val (Num 5))))) = (Exception: IllFormed "can't add non-integers")) 
  "eval_d Binop and Num"; *)

  (* unit_test ((eval_d (Conditional (Var "x", Num 10, Num 20)) (Env.extend (Env.empty ()) "x" (ref (Env.Val (Num 5))))) = (Exception: IllFormed "Condition must be bool")) 
  "eval_d Conditional and Num"; *)

  (* eval_l *)
  (* unit_test (try eval_l (Var "x") (Env.empty ()) = Env.Val (Var "test_var") with
            | EvalError "Unbound variable" -> true
            | _ -> false)
            "eval_l var unbound"; *)
  unit_test ((eval_l (Num (1)) (Env.empty ())) = (Env.Val (Num 1)))
    "eval_l num";
  unit_test ((eval_l (Bool true) (Env.empty ())) = (Env.Val (Bool true)))
    "eval_l Bool";
  unit_test ((eval_l (Unop (Negate, Num 5)) (Env.empty ())) = (Env.Val (Num (-5))))
    "eval_l Unop"; 

  unit_test ((eval_l (Binop (Plus, (Num 3), (Num 5))) (Env.empty ())) = Env.Val (Num 8))
    "eval_l Binop Plus";
  unit_test ((eval_l (Binop (Minus, (Num 3), (Num 5))) (Env.empty ())) = Env.Val (Num (-2)))
    "eval_l Binop Minus";
  unit_test ((eval_l (Binop (Times, (Num 3), (Num 5))) (Env.empty ())) = Env.Val (Num 15))
    "eval_l Binop Times";
  unit_test ((eval_l (Binop (Divide, (Num 15), (Num 3))) (Env.empty ())) = Env.Val (Num 5))
    "eval_l Binop Times";
  unit_test ((eval_l (Binop (Equals, (Num 3), (Num 3))) (Env.empty ())) = Env.Val (Bool true))
    "eval_l Binop is Equal";
  unit_test ((eval_l (Binop (Equals, (Num 3), (Num 5))) (Env.empty ())) = Env.Val (Bool false))
    "eval_l Binop not Equal";
  unit_test ((eval_l (Binop (LessThan, (Num 3), (Num 5))) (Env.empty ())) = Env.Val (Bool true))
    "eval_l Binop is LessThan";
  unit_test ((eval_l (Binop (LessThan, (Num 5), (Num 3))) (Env.empty ())) = Env.Val (Bool false))
    "eval_l Binop not LessThan";
  unit_test ((eval_l (Binop (GreaterThan, (Num 5), (Num 3))) (Env.empty ())) = Env.Val (Bool true))
    "eval_l Binop is GreaterThan";
  unit_test ((eval_l (Binop (And, Bool true, Bool false)) (Env.empty ())) = Env.Val (Bool false))
    "eval_l Binop And";
  unit_test ((eval_l (Binop (Or, Bool true, Bool false)) (Env.empty ())) = Env.Val (Bool true))
    "eval_l Binop And";

  unit_test ((eval_l (Conditional (Bool true, Num (3), Num (5))) (Env.empty ())) = Env.Val (Num 3))
    "eval_l conditional if true";
  unit_test ((eval_l (Conditional (Bool false, Num (3), Num (5))) (Env.empty ())) = Env.Val (Num 5))
    "eval_l conditional if false";
  unit_test ((eval_l (Conditional (Bool true, Num 10, Num 20)) (Env.extend (Env.empty ()) "x" (ref (Env.Val (Num 5))))) = (Env.Val (Num 10))) 
    "eval_l Conditional and Num"; 

  unit_test ((eval_l (Let ("x", Num (3), Var "x")) (Env.empty ())) = Env.Val (Num 3))
    "eval_l let x = 3 in x";
  unit_test ((eval_l (Let ("x", Num (3), Binop (Plus, Var "x", Num 5))) (Env.empty ())) = Env.Val (Num 8))
    "eval_l let x = 3 in x + 5";

  unit_test ((eval_l (Letrec ("x", Num (3), Var "x")) (Env.empty ())) = Env.Val (Num 3))
    "eval_l let rec x = 3 in x";
  unit_test ((eval_l (Letrec ("x", Num (3), Binop (Plus, Var "x", Num 5))) (Env.empty ())) = Env.Val (Num 8))
    "eval_l let rec x = 3 in x + 5";
  unit_test (try (let _ = eval_l (Raise)(Env.empty()) in false) with 
    | EvalException -> true
    | _ -> false)
    "eval_l Raise";
  unit_test (try (let _ = eval_l (Unassigned)(Env.empty()) in false) with 
    | EvalError _ -> true
    | _ -> false)
    "eval_l Unassigned";
  unit_test (eval_l (App (Fun ("x", Num 3), Num 2)) (Env.empty()) = Env.Val (Num 3)) 
    "eval_d App";

  () ;;

let _ = test () ;;
