(* Translate takes AST and produces LLVM IR *)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate prog = 
    (* Get the global variables and functions *)
    let globals = prog.A.v      (* Use this format when referencing records in other modules *)
    and functions = prog.A.f in

    (* Set up Llvm module and context *)
    let context = L.global_context () in 
    let the_module = L.create_module context "ART"
    and i32_t = L.i32_type context
    and i8_t   = L.i8_type   context
    and void_t = L.void_type context in

    (* Function takes ast types and returns corresponding llvm type *)
    let ltype_of_typ = function
        A.Int -> i32_t
      | A.Char -> i8_t
      | A.Void -> void_t
        (* Currently only allowing void and int types *)
      | _   -> raise (Failure "Only valid types are int/char/void") in

    (* This two helper functions suggest that that fdecls needs to be modified *)
    (* Takes a function and return the list of declarations *)
    let locals_of_fdecl fdecl = match fdecl.A.body with
        Block(decls, stmts) -> decls
      | _ -> raise (Failure "Illegal function block") (* guaranteed to never happen *)
    and body_of_fdecl fdecl = match fdecl.A.body with
        Block(decls, stmts) -> stmts
      | _ -> raise (Failure "Illegal function block") (* guaranteed to never happen *)
    in


    (* Declaring each global variable and storing value in a map.
       global_vars is a map of var names to llvm global vars representation.
       Global decls are three tuples (typ, name, initer) *)
    let global_vars =
        let global_var m (t,n,i) = (* map (type,name,initer) *)
        (* Ignoring initer for now and just setting to zero *)
          let init = L.const_int (ltype_of_typ t) 0
          (* Define the llvm global and add to the map *)
          in StringMap.add n (L.define_global n init the_module) m in
        List.fold_left global_var StringMap.empty globals in

    (* Declare printf() *)
    (* Allowing a print builtin function for debuggin purposes *)
    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func = L.declare_function "printf" printf_t the_module in

    (* Defining each of the declared functions *)
    (* Function decls is a map from function names to tuples of llvm function representation
      and Ast declarations *)
    let function_decls =
        let function_decl m fdecl =
          let name = fdecl.A.name
          and formal_types = (* Types of parameters in llvm type repr *)
            (* Note: Ignoring pass by reference for now *)
            Array.of_list (List.map (fun (t,_,_) -> ltype_of_typ t) fdecl.A.params)
            (* ftype from return type and formal_types *)
          in let ftype = L.function_type (ltype_of_typ fdecl.A.rettyp) formal_types in
          StringMap.add name (L.define_function name ftype the_module, fdecl) m in
        List.fold_left function_decl StringMap.empty functions in

   (* Fill in the body of the given function *)
    let build_function_body fdecl =
        (* Get the corresponding llvm function value *)
        let (the_function, _) = StringMap.find fdecl.A.name function_decls in
        (* Get an instruction builder that will emit instructions in the current function *)
        let builder = L.builder_at_end context (L.entry_block the_function) in

        (* Format strings for printf call *)
        let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
        let char_format_str = L.build_global_stringptr "%c" "fmt" builder in

        (* Construct the function's "locals": formal arguments and locally
           declared variables.  Allocate each on the stack, initialize their
           value, if appropriate, and remember their values in the "locals" map *)
        let local_vars =
          (* Arguments: map (type, name) param  (llvm  of params)*)
          let add_formal m (t, n) p = L.set_value_name n p;
          (* name appended with nums as necessary: eg  a1,a2 *)
          (* Look at microc lecture: pg 39 *)
            let local = L.build_alloca (ltype_of_typ t) n builder in (* allocate stack for params *)
            ignore (L.build_store p local builder); (* Store the param in the allocated place *)
            StringMap.add n local m in (* We add the stack version *)
          let add_local m (t,n) =
            let local_var = L.build_alloca (ltype_of_type t) n builder (* allocate space for local *)
            in StringMap.add n local_var m in

          let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
            (Array.to_list (L.params the_function)) in
          List.fold_left add_local formals (locals_of_fdecl fdecl) in

        (* Return the value for a variable or formal argument *)
        (* Note: this checks local scope before global. We have to do more complicated scoping *)
        let lookup n = try StringMap.find n local_vars
                       with Not_found -> StringMap.find n global_vars
        in

        (* Construct code for an expression; return its value *)

        let rec expr builder = function (* Takes args builder and Ast.expr *)
            A.IntLit i -> L.const_int i32_t i
          | A.CharLit c -> L.const_int i8_t c (* 1 byte characters *)
          | A.Noexpr -> L.const_int i32_t 0  (* No expression is 0 *)
          | A.ID s -> L.build_load (lookup s) s builder (* Load the variable into register and return register *)
          | A.Binop (e1, op, e2) ->
              let e1' = expr builder e1
              and e2' = expr builder e2 in
              match op with
                  A.Add     -> L.build_add  e1' e2' "tmp" builder
                | A.Sub     -> L.build_sub  e1' e2' "tmp" builder
                | A.Mult    -> L.build_mul  e1' e2' "tmp" builder
                | A.Div     -> L.build_sdiv e1' e2' "tmp" builder
                | A.Mod     -> L.build_srem e1' e2' "tmp" builder
                | A.And     -> L.build_and  e1' e2' "tmp" builder
                | A.Or      -> L.build_or   e1' e2' "tmp" builder

                | A.Equal   -> L.build_icmp L.Icmp.Eq  e1' e2' "tmp" builder
                | A.Neq     -> L.build_icmp L.Icmp.Ne  e1' e2' "tmp" builder
                | A.Less    -> L.build_icmp L.Icmp.Slt e1' e2' "tmp" builder
                | A.Leq     -> L.build_icmp L.Icmp.Sle e1' e2' "tmp" builder
                | A.Greater -> L.build_icmp L.Icmp.Sgt e1' e2' "tmp" builder
                | A.Geq     -> L.build_icmp L.Icmp.Sge e1' e2' "tmp" builder

                (* Need to move assignment away from bin *)
                | A.Asn -> ignore (L.build_store e2 (lookup
                        (match e1 with (* Only works with ID *)
                          A.ID s -> s
                        | _   -> raise (Failure "Assignment only allowed to identifier") )
                    ) builder ); e1
                | _  -> raise (Failure "Unsupported binary op")(* Ignore other binary ops *)
          | A.Unop(op, e) ->
              let e' = expr builder e in
              (match op with
                A.Neg     -> L.build_neg
              | A.Not     -> L.build_not) e' "tmp" builder
              | _  -> raise (Failure "Unsupported unary op")(* Ignore other unary ops *)

          | A.Call (ID "printi", [e]) -> L.build_call print_func [|int_format_str ; (expr builder e) |] "printf" builder
          | A.Call (ID "printc", [e]) -> L.build_call print_func [|char_format_str ; (expr builder e) |] "printf" builder
          | A.Call (ID f, act) ->
             let (fdef, fdecl) = StringMap.find f function_decls in
             let actuals = List.rev (List.map (exr builder) (List.rev act)) in
             let result = (match fdecl.A.typ with A.Void -> ""
                                                | _ -> f^"_result") in
             L.build_call fdef (Array.of_list actuals) result builder
          |  _  -> raise (Failure "Unsupported expression")(* Ignore other expressions *)
        in
    the_module