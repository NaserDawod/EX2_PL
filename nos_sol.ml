[@@@ocaml.warning "-8"];;

let rec nos c = match c with 
              (Ast_sol.Ass (x, e), s) -> Semantics_sol.update x e s
            | (Skip, s) -> s
            | (Comp (s1, s2), s) -> nos (s2, nos(s1, s))
            | (If (b, s1, s2), s) -> if Semantics_sol.solve_b b s = "tt" then nos (s1, s) else nos (s2, s)
            | (While (b, s1), s) -> if Semantics_sol.solve_b b s = "tt" then nos (While (b, s1),nos (s1, s)) else nos (Skip, s)
            | (Do_While (b, s1), s) -> if Semantics_sol.solve_b b s = "tt" then nos (Do_While (b, s1), nos (s1, s))
                                                                       else nos (s1, s);;

(* tests *) 

print_string "x = ";;
print_int (let new_state = nos (Ast_sol.test1, Semantics_sol.s0) in new_state "x");;
print_endline "";;

print_string "x = ";;
print_int (let new_state = nos (Ast_sol.test2, Semantics_sol.s0) in new_state "x");;
print_endline "";;

print_string "x = ";;
print_int (let new_state = nos (Ast_sol.test3, Semantics_sol.s0) in new_state "x");;
print_endline "";;

print_string "x = ";;
print_int (let new_state = nos (Ast_sol.test4, Semantics_sol.s1) in new_state "x");;
print_endline "";;

print_string "y = ";;
print_int (let new_state = nos (Ast_sol.test4, Semantics_sol.s1) in new_state "y");;
print_endline "";;

print_string "a = ";;
print_int (let new_state = nos (Ast_sol.test5, Semantics_sol.s2) in new_state "a");;
print_endline "";;


