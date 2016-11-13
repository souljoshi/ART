(* Translate takes AST and produces LLVM IR *)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate prog = 
    (* Get the global variables and functions *)
    let globals = prog.A.v      (* Use this format when referencing records in other modules *)
    and functions = prog.A.f
    and structs = prog.A.s in

    (* Set up Llvm module and context *)
    let context = L.global_context () in 
    let the_module = L.create_module context "ART"
    and i32_t = L.i32_type context
    and i8_t   = L.i8_type   context
    and void_t = L.void_type context in

    (* General verson of lltype_of_typ that takes a struct map *)
    (* The struct map helps to get member types for structs in terms of previously *)
    (* defined structs *)
    let rec _ltype_of_typ m = function
        A.Int -> i32_t
      | A.Char -> i8_t
      | A.Void -> void_t
      | A.Array(t,e) -> (match e with 
            |A.IntLit(i) -> L.array_type (_ltype_of_typ m t) i
            | _ -> raise(Failure "Arrays declaration requires int literals for now"))
      | A.UserType(s,_) -> StringMap.find s m
        (* Currently supporting only void, int and struct types *)
      | _   -> raise (Failure "Only valid types are int/char/void")

    in

    (* Defining each of the structs *)
    (* struct_ltypes is a map from struct names to their corresponing llvm type. *)
    (* It's used by ltype_of_typ. It has to be defined this way to allow structs to *)
    (* have member whose type is of previously defined structs *)
    let struct_ltypes =
        let struct_ltype m st =
          (* Ocaml Array containing llvm lltypes of the member variables *)
          let decls_array = Array.of_list( List.rev ( List.fold_left
                  (fun l (t,_) -> (_ltype_of_typ m t)::l) [] st.A.decls) )
          (* Define the llvm struct type *)
        in let named_struct = L.named_struct_type context st.A.sname
        in  L.struct_set_body named_struct decls_array false ; (* false -> not packed *)
        StringMap.add st.A.sname named_struct m in
        List.fold_left struct_ltype StringMap.empty structs
    in

    (* Function takes ast types and returns corresponding llvm type *)
    let ltype_of_typ t = _ltype_of_typ struct_ltypes t

    in
    (* Declaring each global variable and storing value in a map.
       global_vars is a map of var names to llvm global vars representation.
       Global decls are three tuples (typ, name, initer) *)
    let global_vars =
        let global_var m (t,n,i) = (* map (type,name,initer) *)
        (* Ignoring initer for now and just setting to zero *)
          let init = L.const_int (ltype_of_typ t) 0
          (* Define the llvm global and add to the map *)
          in StringMap.add n (L.define_global n init the_module, t) m in
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
          let name = fdecl.A.fname
          and formal_types = (* Types of parameters in llvm type repr *)
            Array.of_list (List.map (fun (t,_,pass) ->
                        let lt = ltype_of_typ t in
                        match pass with
                          A.Value -> lt
                        | A.Ref  -> L.pointer_type lt (* If pass by reference use pointers *)
                                ) fdecl.A.params)
            (* ftype from return type and formal_types *)
          in let ftype = L.function_type (ltype_of_typ fdecl.A.rettyp) formal_types in
          StringMap.add name (L.define_function name ftype the_module, fdecl) m in
        List.fold_left function_decl StringMap.empty functions in

    (* Map from struct names to tuples (member variable map, methods map, lltype) *)
    let struct_decls =
      (* struct_decl takes a map and an ast sdecl and returns a map which contains sdecl added *)
      let struct_decl m sdecl =
        let lstype = ltype_of_typ (A.UserType(sdecl.A.sname, sdecl.A.ss)) in
        (* Map from struct member variable name to (ast type, index) *)
        (* index refers to the position of member in struct and is used for calling getelementptr *)
        (* Note: members in the ast have variant type "bind = typ * string" *)
        let varmap =
          let varindex = List.rev ( snd (List.fold_left (fun (i,l) (t,n) -> ( i+1,(n,t,i)::l) )
              (0,[]) sdecl.A.decls) ) in
          List.fold_left (fun vm (n,t,i) -> StringMap.add n (t,i) vm ) StringMap.empty varindex
        in
        (* Map from method/construct name to (llvm func type, fdecl).*)
        (* Similar to the function_decls map.*)
        let methodmap =
          let method_decl m fdecl =
            let name = fdecl.A.fname
            (* Append a pointer to the current struct type to parameter list.
              It will be used as a this pointer. Method calls always implicitly
              pass the caller as first argument *)
            and formal_types = Array.of_list
            ((L.pointer_type lstype)::(List.map (fun (t, _, pass) ->
                  let lt = ltype_of_typ t in
                  match pass with
                    A.Value -> lt
                  | A.Ref  -> L.pointer_type lt (* If pass by reference use pointers *)
                          ) fdecl.A.params) ) in
            (* NOTE: Return type for constructor is Void *)
            let ret_type = ltype_of_typ fdecl.A.rettyp
            in let ftype = L.function_type ret_type formal_types in
            StringMap.add name (L.define_function name ftype the_module, fdecl) m in
        List.fold_left method_decl StringMap.empty (sdecl.A.ctor::sdecl.A.methods)
      in

        StringMap.add sdecl.A.sname (varmap, methodmap, lstype) m in
      List.fold_left struct_decl StringMap.empty structs

    in

    (* Returns (fdef, fdecl) for a method in constructor *)
    let lookup_method sname fname =
      let (_,methodmap,_) = StringMap.find sname struct_decls in
      StringMap.find fname methodmap
    in
    (* Returns index of member memb in struct named sname *)
    let memb_index sname memb =
      (* Obtain varmap from struct_decls map *)
      let (varmap, _,_) = try StringMap.find sname struct_decls
                    with Not_found -> raise (Failure("Varmap not found for : "^sname^"."^memb))
      in
      (* Obtain index from varmap *)
      let (_, i) = try StringMap.find memb varmap
                    with Not_found -> raise (Failure("Index not found for : "^sname^"."^memb))
                    in i
    in

   (* Fill in the body of the given function *)
    let build_function_body fdecl =
        (* Get the corresponding llvm function value *)
        let (the_function, _) = (match fdecl.A.typ with
                  A.Func -> StringMap.find fdecl.A.fname function_decls
                  | _ -> lookup_method fdecl.A.owner fdecl.A.fname
              ) in
        (* Get an instruction builder that will emit instructions in the current function *)
        let builder = L.builder_at_end context (L.entry_block the_function) in

        (* Format strings for printf call *)
        let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
        let char_format_str = L.build_global_stringptr "%c" "fmt" builder in

        (* Construct the function's "locals": formal arguments and locally
           declared variables.  Allocate each on the stack, initialize their
           value, if appropriate, and remember their values in the "locals" map *)
        let local_vars =
          (* Arguments: map (type, name, pass) param  (llvm  of params)*)
          let add_formal m (t, n,pass) p = L.set_value_name n p;
          (* name appended with nums as necessary: eg  a1,a2 *)
          (* Look at microc lecture: pg 39 *)
            let local =  match pass with
                A.Value -> L.build_alloca (ltype_of_typ t) n builder (* allocate stack for value params *)
             |  A.Ref  -> p

            in
            ignore (match pass with
                A.Value -> ignore(L.build_store p local builder); (* Store the value param in the allocated place *)
             |  A.Ref -> () );

            StringMap.add n (local,t) m in (* We add the stack version *)
          let add_local m (t,n) =
            let local_var = L.build_alloca (ltype_of_typ t) n builder (* allocate space for local *)
            in StringMap.add n (local_var,t) m in

          (* llvm type list for the params *)
          let lparams = (match fdecl.A.typ with
                    A.Func -> Array.to_list (L.params the_function)
                  (* For Method/Const drop the "this" param as it needs to be inaccessible to user *)
                  | _ -> List.tl (Array.to_list (L.params the_function)))

          in
          let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.params lparams
          in
          List.fold_left add_local formals (List.map (fun (a,b,_) -> (a,b)) fdecl.A.locals) in

        (* Returns (fdef, fdecl) for a function call in method/function *)
        (* Handles case of constructor call, a method call without dot operator and
           normal function call *)
        (* Second case can happen only within struct scope when calling a struct's method
           from another method of the same struct *)
        let lookup_function f =
          (* First try to find a matching constructor *)
          try lookup_method f f
          (* If that fails try to find a method.
             this is guaranteed to fail in a normal function *)
          with Not_found -> (try lookup_method fdecl.A.owner f
          (* Finally look for normal function *)
                with Not_found -> StringMap.find f function_decls)
        in

        (* Return the value for a variable or formal argument *)
        (* Note: this checks local scope before global. We have to do more complicated scoping *)
        let lookup n builder =
            (* First try to find a local variable *)
            try fst(StringMap.find n local_vars)
            (* Then try to find a member, if not member jump to global variable handling *)
            with Not_found -> try (match fdecl.A.typ with
                A.Func -> raise Not_found (* Functions can't access members *)
              |_-> let ind = try memb_index fdecl.A.owner n with Failure s -> raise Not_found in
                  (* If member, get its pointer by dereferencing the first argument
                    corresponding to the "this" pointer *)
                  let e' = L.param (fst (lookup_function fdecl.A.fname) ) 0  in
                  L.build_gep e' [|L.const_int i32_t 0; L.const_int i32_t ind |] "tmp" builder
            ) with Not_found -> fst(StringMap.find n global_vars)

        in

        (* Looks up type of local variables *)
        let lookup_type n = try snd(StringMap.find n local_vars)
                       with Not_found -> snd(StringMap.find n global_vars)
        in

        (* Like string_of_typ but prints "circle" instead of "shape circle" *)
        let string_of_typ2 = function
            A.UserType(s, _) -> s
          | t -> A.string_of_typ t

        in

        (* Returns a tuple (type name, ast type) for an expression *)
        (* In final code [with semantic analysis] the ast type should be part of expr *)
        let rec expr_type  = function
          A.IntLit i -> ("int", A.Int)
        | A.Id s -> let t =  lookup_type s in (string_of_typ2 t, t)
        | A.Index(a, e) -> (match snd(expr_type a) with (* First get type of the expr being indexed *)
                            (* The type of the index expression is the subtype 't' of the array *)
                            A.Array (t, e2) -> (string_of_typ2 t, t)
                          | _ -> raise (Failure ("Indexing non array")))

        | A.Member(e, s) -> let (n,t) = expr_type e (* Get type name of the expression before dot *)
                      (* Look for it in the structs map and get the var map *)
                      in  let (varmap, _,_ ) = StringMap.find n struct_decls
                      (* Look for string after the dot in the varmap *)
                      in let t = fst(StringMap.find s varmap)
                      in (string_of_typ2 t, t)
        |_ -> raise (Failure ("Unsupported Expression for expr_type"))

        in

        (* Construct code for an lvalue; return a pointer to access object *)
        let rec lexpr builder = function
            A.Id s -> lookup s builder
          | A.Index(e1,e2) -> let e2' = expr builder e2 in
                L.build_gep (lexpr builder e1) [|L.const_int i32_t 0; e2'|] "tmp" builder
          | A.Member(e, s) -> let e' = lexpr builder e in
              (* Obtain index of s in the struct type of expression e *)
              let (sname, _ ) = expr_type e in let i = memb_index sname s in
              L.build_gep e' [|L.const_int i32_t 0; L.const_int i32_t i|] "tmp" builder
          | _ -> raise (Failure "Trying to assign to an non l-value")


        (* Construct code for an expression; return its value *)
        and expr builder = function (* Takes args builder and Ast.expr *)
            A.IntLit i -> L.const_int i32_t i
          | A.CharLit c -> L.const_int i8_t (int_of_char c) (* 1 byte characters *)
          | A.Noexpr -> L.const_int i32_t 0  (* No expression is 0 *)
          | A.Id s -> L.build_load (lookup s builder) s builder (* Load the variable into register and return register *)
          | A.Binop (e1, op, e2) ->
              let e1' = expr builder e1
              and e2' = expr builder e2 in
              (match op with
                  A.Add     -> L.build_add
                | A.Sub     -> L.build_sub
                | A.Mult    -> L.build_mul
                | A.Div     -> L.build_sdiv
                | A.Mod     -> L.build_srem
                | A.And     -> L.build_and
                | A.Or      -> L.build_or

                | A.Equal   -> L.build_icmp L.Icmp.Eq
                | A.Neq     -> L.build_icmp L.Icmp.Ne
                | A.Less    -> L.build_icmp L.Icmp.Slt
                | A.Leq     -> L.build_icmp L.Icmp.Sle
                | A.Greater -> L.build_icmp L.Icmp.Sgt
                | A.Geq     -> L.build_icmp L.Icmp.Sge
              ) e1' e2' "tmp" builder

          | A.Index(e1,e2) as arr-> L.build_load (lexpr builder arr) "tmp" builder

          | A.Member(e, s) as mem -> L.build_load (lexpr builder mem) "tmp" builder

          | A.Asnop (el, op, er) ->
               let el' = lexpr builder el in
               (match op with
                   A.Asn -> let e' = expr builder er in
                             ignore (L.build_store e' el' builder); e'
                   (* The code here must change if supporting non-identifiers *)
                 | A.CmpAsn bop -> let e' = expr builder (A.Binop(el, bop, er)) in
                             ignore (L.build_store e' el' builder); e'
               )

          | A.Unop(op, e) ->
              let e' = expr builder e in
              (match op with
                A.Neg     -> L.build_neg
              | A.Not     -> L.build_not
              | _  -> raise (Failure "Unsupported unary op")(* Ignore other unary ops *)
                ) e' "tmp" builder


            (* This ok only for few built_in functions *)
          | A.Call (A.Id "printi", [e]) -> L.build_call printf_func [|int_format_str ; (expr builder e) |] "printf" builder
          | A.Call (A.Id "printc", [e]) -> L.build_call printf_func [|char_format_str ; (expr builder e) |] "printf" builder
            (* A call without a dot expression refers to three possiblities. In order of precedence: *)
            (* constructor call, method call (within struct scope), function call *)
          | A.Call (A.Id f, act) ->
             (* The llvm type array of the calling functions parameters
                Can be use to retreive the "this" argument *)
             let myparams  = L.params (fst (lookup_function fdecl.A.fname) ) in
             let (fdef, fdecl) = lookup_function f in
             (* Helper function for pass by value handling *)
             let arg_passer builder (_,_,pass) = function
                (* Identifier, index, and member expressions may be passed by reference.
                   Other types are required to be passed by value. *)
                A.Id(_) | A.Index(_,_) | A.Member(_,_) as e -> (match pass with
                          A.Ref -> lexpr builder e (* This gets the pointer to the variable *)
                        | A.Value -> expr builder e )
              | e  -> expr builder e
             in
             (* This makes right to left evaluation order. What order should we use? *)
             let actuals = List.rev (List.map2 (arg_passer builder) (List.rev fdecl.A.params) (List.rev act)) in
             let result = (match fdecl.A.rettyp with A.Void -> "" (* don't name result for void llvm issue*)
                                                | _ -> f^"_result") in
             (* How the function is called depends on the type *)
             ( match fdecl.A.typ with
                A.Func -> L.build_call fdef (Array.of_list actuals) result builder
                (* For methods pass value  of the callers "this" argument *)
              | A.Method -> L.build_call fdef (Array.of_list (myparams.(0) :: actuals)) result builder
                (* Constructors are called like methods but evaluate to their own type
                  not their return value as they don't have explicit return values *)
              | A.Constructor -> let (_,_,lstype) =  StringMap.find f struct_decls
                          (* Create local temporary to hold newly created struct *)
                          in let loc = L.build_alloca  lstype "tmp" builder in
                          (* Pass the newly created struct as the "this" arugment of constructor call *)
                         ignore( L.build_call fdef (Array.of_list (loc :: actuals)) result builder);
                          (* Return the initialized local temporary *)
                         L.build_load  loc "tmp" builder
              )
          (* Explicit method calls with dot operator *)
          | A.Call (A.Member(e,s), act) ->
             let (sname, _ ) = expr_type e in
             let (fdef, fdecl) = lookup_method sname s in
             (* Helper function for pass by value handling *)
             (* Same us the above code *)
             let arg_passer builder (_,_,pass) = function
                A.Id(_) | A.Index(_,_) | A.Member(_,_) as e -> (match pass with
                          A.Ref -> lexpr builder e (* This gets the pointer to the variable *)
                        | A.Value -> expr builder e )
              | e  -> expr builder e
            in
             (* This makes right to left evaluation order. What order should we use? *)
             let actuals = List.rev (List.map2 (arg_passer builder) (List.rev fdecl.A.params) (List.rev act)) in
             (* Append the left side of dot operator to arguments so it is used as a "this" argument *)
             let actuals = (lexpr builder e )::actuals in
             let result = (match fdecl.A.rettyp with A.Void -> "" (* don't name result for void llvm issue*)
                                                | _ -> s^"_result") in
             L.build_call fdef (Array.of_list actuals) result builder
          |  e -> raise (Failure ("Unsupported expression: "^(A.string_of_expr e)))(* Ignore other expressions *)
        in
        (* Invoke "f builder" if the current block doesn't already
           have a terminal (e.g., a branch). *)
        let add_terminal builder f =
            match L.block_terminator (L.insertion_block builder) with
                Some _ -> ()
              | None -> ignore (f builder) in

        (* Build the code for the given statement; return the builder for
         the statement's successor *)
        let rec stmt builder = function
              A.Block (vl, sl) -> List.fold_left stmt builder sl (* Ignore the decls for now *)
            | A.Expr e -> ignore (expr builder e); builder  (* Simply evaluate expression *)

            | A.Return e -> ignore (match fdecl.A.rettyp with  (* Different cases for void and non-void *)
                A.Void -> L.build_ret_void builder
                | _ -> L.build_ret (expr builder e) builder); builder
            | A.If (predicate, then_stmt, else_stmt) ->
                let bool_val = expr builder predicate in
                let merge_bb = L.append_block context "merge" the_function in (* Merge block *)
                let then_bb = L.append_block context "then" the_function in
                (* Get the builder for the then block, emit then_stmt and then add branch statement to
                    merge block *)
                add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
                    (L.build_br merge_bb);

                let else_bb = L.append_block context "else" the_function in
                add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
                    (L.build_br merge_bb);
                (* add_terminal used to avoid insert two terminals to basic blocks *)

                (* builder is in block that contains if stmt *)
                ignore (L.build_cond_br bool_val then_bb else_bb builder);
                L.builder_at_end context merge_bb (* Return builder at end of merge block *)

            | A.While (predicate, body) ->
                let pred_bb = L.append_block context "while" the_function in
                ignore (L.build_br pred_bb builder); (* builder is in block that contains while stm *)

                let body_bb = L.append_block context "while_body" the_function in
                add_terminal (stmt (L.builder_at_end context body_bb) body)
                (L.build_br pred_bb);

                let pred_builder = L.builder_at_end context pred_bb in
                let bool_val = expr pred_builder predicate in

                let merge_bb = L.append_block context "merge" the_function in
                ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
                L.builder_at_end context merge_bb

            (*  make equivalent while *)
            | A.For (e1, e2, e3, body) -> stmt builder
                ( A.Block ( [], [  A.Expr e1; A.While (e2, A.Block ([], [body; A.Expr e3]) ) ] ) )
            | _  -> raise (Failure "Unsupported statement type")(* Ignore other statement type *)
        in
        (* Build the code for each statement in the function *)
        let builder = stmt builder (A.Block(fdecl.A.locals, fdecl.A.body)) in

        (* Add a return if the last block falls off the end *)
        add_terminal builder (match fdecl.A.rettyp with
            A.Void -> L.build_ret_void
          | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
    in

    List.iter build_function_body functions;
    (* Build methods and constructors for each struct *)
    List.iter (fun sdecl -> List.iter build_function_body (sdecl.A.ctor::sdecl.A.methods)) structs;
    the_module
