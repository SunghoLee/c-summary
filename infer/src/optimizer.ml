
open! IStd
open Core
module F = Format
module L = Logging
module Domain = SemanticSummaryDomain
open Domain

module SMTSolver : sig
    val is_sat : Cst.t -> bool * Cst.t
end = struct
    include Z3

    let debug = false 

    let print cst expr sim_expr sim_cst result = 
        if debug then
           L.progress "\tEXPR: %s \n\t -> %s \n\t -> %s \n\t -> %s \n\t -> %s\n@." (Cst.pp cst) (Expr.to_string expr) (Expr.to_string sim_expr) (Cst.pp sim_cst) result

    let ctx = 
        let cfg = [("model", "true")] in
        mk_context cfg

    let is_sat cst = 
        let expr = Cst.encode ctx cst in
        let sim_expr = Expr.simplify expr None in
        let sim_cst = Cst.decode sim_expr in
        let solver = Solver.mk_solver ctx None in
        let res = Solver.check solver [sim_expr] in
        match res with
        | UNSATISFIABLE -> print cst expr sim_expr sim_cst "UNSAT"; (false, sim_cst)
        | UNKNOWN -> print cst expr sim_expr sim_cst "UNKNOWN"; (true, sim_cst)
        | _ -> print cst expr sim_expr sim_cst "SAT"; (true, sim_cst)
end 

module LocSet = struct 
    include Caml.Set.Make(AbsLoc)
    let pp fmt s =
        F.fprintf fmt "[%s]" (fold (fun x i -> i ^ ", " ^ (AbsLoc.pp x)) s "")

    let to_string s =
        Printf.sprintf "[%s]" (fold (fun x i -> i ^ ", " ^ (AbsLoc.pp x)) s "")
end

module RevEnv = struct
    include Caml.Map.Make(AbsLoc)

    let rev env = 
        Env.fold (fun x y e -> add y x e) env empty
end

module RevDG = struct
    include Caml.Map.Make(AbsLoc)

    let add from_loc to_loc dg = 
        match find_opt to_loc dg with
        | None -> 
            let from_set = LocSet.add from_loc LocSet.empty in
            add to_loc from_set dg
        | Some s -> 
            let from_set = LocSet.add from_loc s in
            add to_loc from_set dg

    let fold = fold

    let pp fmt dg = 
        F.fprintf fmt "[%s]" (fold (fun x y i -> i ^ ", " ^ (AbsLoc.pp x) ^ " -> " ^ (LocSet.to_string y)) dg "")
end

let make_rev_dg heap = 
    let f from to_set dg = 
       AbstractValueSet.fold (fun (av, cst) dg -> 
           if AbsVal.is_loc av then RevDG.add from (AbsVal.val_to_loc av) dg
           else dg) to_set dg
    in
    Heap.fold f heap RevDG.empty

let calc_from_closure loc dg = 
    let rec impl loc acc dg = 
        match RevDG.find_opt loc dg with
        | Some from_set -> 
                LocSet.fold 
                (fun from_loc acc -> 
                    if LocSet.mem from_loc acc then acc 
                    else impl from_loc (LocSet.add from_loc acc) dg) from_set acc
        | None -> acc
    in
    impl loc (LocSet.add loc LocSet.empty) dg

let opt_heap heap env =
    let rev_env = RevEnv.rev env in
    let rev_dg = make_rev_dg heap in
    let f loc _ = 
        calc_from_closure loc rev_dg |>
        LocSet.for_all (fun loc -> match RevEnv.find_opt loc rev_env with None -> true | Some v -> not (Var.is_temporal v))
    in
    Heap.filter f heap

let opt_logs logs = 
    let opt_unit u = 
        let args = LogUnit.get_args u in
        let heap = LogUnit.get_heap u in
        let rev_dg = make_rev_dg heap in
        let f loc _ = 
            calc_from_closure loc rev_dg |>
            LocSet.for_all (fun loc -> if LocList.mem loc args then true else false)
        in
        {u with LogUnit.heap = Heap.filter f heap}
    in
    CallLogs.map opt_unit logs

let remove_redundant state = 
    let (env, heap, logs) = Domain.to_triple state in
    let heap' = opt_heap heap env in
    let env' = Env.filter (fun v loc -> not (Var.is_temporal v)) env in
    let logs' = opt_logs logs in
    Domain.make env' heap' logs'

let optimize astate = 
    let astate' = remove_redundant astate in
    let (env, heap, logs) = Domain.to_triple astate' in
    let opt_heap l v h = 
        let opt_abs_set (v, cst) i =
            match SMTSolver.is_sat cst with
            | (true, cst') -> AbstractValueSet.add (v, cst') i
            | _ -> i
        in
        let v' = AbstractValueSet.fold opt_abs_set v AbstractValueSet.empty in
        if AbstractValueSet.is_empty v' then
            h
        else
            Heap.add l v' h
    in 
    let opt_logs l = 
        let h = l.LogUnit.heap in
        let h' = Heap.fold opt_heap h Heap.empty in
        { l with LogUnit.heap = h' }
    in
    let heap' = Heap.fold opt_heap heap Heap.empty in
    let logs' = CallLogs.map opt_logs logs in
    Domain.make env heap' logs'
