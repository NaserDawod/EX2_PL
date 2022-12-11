[@@@ocaml.warning "-8"];;

type config = Inter of Ast_sol.stm * Ast_sol.state 
            | Final of Ast_sol.state;;

let stop c = match c with 
            (Inter (_, _)) -> true
            | Final (s) -> false;;

let rec sos c = match c with 
                (Ast_sol.Ass (x, e), s) -> Final (Semantics_sol.update x e s)
                | (Skip, s) -> Final s
                | (Comp (s1, s2), s) ->  if stop (sos (s1, s)) 
                      then (let Inter (s1',s') = sos(s1, s) in Inter (Comp(s1', s2), s')) 
                      else (let Final s' = sos(s1, s) in Inter (s2, s'))   
                | (If (b, s1, s2), s) -> if Semantics_sol.solve_b b s = "tt" then (Inter (s1, s)) else (Inter (s2, s))
                | (While (b, s1), s) -> Inter (If (b, Comp(s1, While(b, s1)), Skip), s)
                | (Do_While (b, s1), s) -> Inter (Comp (s1, If (b, Do_While (b, s1), Skip)), s)

 let rec run_sos c = if stop (sos c) 
                      then (let Inter (s,s0) = sos c in run_sos (s,s0))
                      else sos c;;

(* tests *) 

print_string "x = ";;
print_int (let Final state = run_sos (Ast_sol.test1, Semantics_sol.s0) in state "x");;
print_endline "";;

print_string "x = ";;
print_int (let Final state = run_sos (Ast_sol.test2, Semantics_sol.s0) in state "x");;
print_endline "";;

print_string "x = ";;
print_int (let Final state = run_sos (Ast_sol.test3, Semantics_sol.s0) in state "x");;
print_endline "";;

print_string "x = ";;
print_int (let Final state = run_sos (Ast_sol.test4, Semantics_sol.s1) in state "x");;
print_endline "";;

print_string "y = ";;
print_int (let Final state = run_sos (Ast_sol.test4, Semantics_sol.s1) in state "y");;
print_endline "";;

print_string "a = ";;
print_int (let Final state = run_sos (Ast_sol.test5, Semantics_sol.s2) in state "a");;
print_endline "";;
