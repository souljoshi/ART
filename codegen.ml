(* Translate takes AST and produces LLVM IR *)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)
module StringSet = Set.Make( struct let compare = Pervasives.compare type t = string end )

(* scope types *)
type scope = GlobalScope | LocalScope | StructScope | ClosureScope

let translate prog = 
    (* Get the global variables and functions *)
    let globals = prog.A.v      (* Use this format when referencing records in other modules *)
    and functions = prog.A.f
    and structs = prog.A.s in

    (* Set up Llvm module and context *)
    let context = L.global_context () in 
    let the_module = L.create_module context "ART"
    and i32_t = L.i32_type context
    and i64_t = L.i64_type context
    and i8_t   = L.i8_type   context
    and void_t = L.void_type context
    and double_t = L.double_type context
    in
    let string_t = L.pointer_type i8_t
    and i8ptr_t = L.pointer_type i8_t
    and i8ptrptr_t = L.pointer_type (L.pointer_type i8_t)
    and vec_t = L.vector_type double_t 2
    in

    (* General verson of lltype_of_typ that takes a struct map *)
    (* The struct map helps to get member types for structs in terms of previously *)
    (* defined structs *)
    let rec _ltype_of_typ m = function
        A.Int -> i32_t
      | A.Char -> i8_t
      | A.Void -> void_t
      | A.Float -> double_t
      | A.String -> string_t
      | A.Vec -> vec_t
      | A.Array(t,e) -> (match e with 
            | A.IntLit(i) -> L.array_type (_ltype_of_typ m t) i
            | _ -> raise(Failure "Arrays declaration requires int literals for now"))
      | A.UserType(s,_) -> StringMap.find s m
        (* Currently supporting only void, int and struct types *)
      | _   -> raise (Failure "Only valid types are int/char/void/string/double/vec")

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
          let init = L.const_null(ltype_of_typ t)
          (* Define the llvm global and add to the map *)
          in StringMap.add n (L.define_global n init the_module, t) m in
        List.fold_left global_var StringMap.empty globals in

    let timeval_struct_t = let g = L.named_struct_type context "timeval"
        in  ignore(L.struct_set_body g [| i64_t ; i64_t |] false ); g
    in
    (* Declare printf() *)
    (* Allowing a print builtin function for debuggin purposes *)
    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |]



    (* GLUT FUNCTION ARG TYPES *)
    and  glut_init_t = L.function_type void_t[|L.pointer_type i32_t;L.pointer_type(L.pointer_type i8_t)|]
    and  glut_crwin_t = L.function_type i32_t [| L.pointer_type i8_t |]
    and  get_tday_t = L.function_type i32_t [| L.pointer_type timeval_struct_t ; L.pointer_type i8_t|]
    and  void_void_t = L.function_type void_t[|  |]
    and  void_i8p_t = L.function_type void_t[| i8ptr_t |]
    and  void_int_t  = L.function_type  void_t [| i32_t |]
    and  void_int_int_t  = L.function_type  void_t [| i32_t ; i32_t|]
    and  void_2d_t  = L.function_type  void_t [| double_t ; double_t|]
    and  void_3d_t  = L.function_type  void_t [| double_t ; double_t; double_t|]
    and  double_double_t = L.function_type double_t [|double_t|]
    in
    let  void_callback_t  = L.function_type void_t[| L.pointer_type void_void_t |]
  in

    (* END OF GLUT ARG TYPES *)
    let printf_func = L.declare_function "printf" printf_t the_module 
    and get_tday_func = L.declare_function "gettimeofday" get_tday_t the_module

    (* GLUT FUNCTION DELCARATIONS *)
    and glutinit_func      = L.declare_function "glutInit" glut_init_t the_module
    and glutinitdmode_func = L.declare_function "glutInitDisplayMode" void_int_t the_module
    and glutinitwpos_func  = L.declare_function "glutInitWindowPosition" void_int_int_t the_module
    and glutinitwsiz_func  = L.declare_function "glutInitWindowSize" void_int_int_t the_module
    and glutcreatewin_func = L.declare_function "glutCreateWindow" glut_crwin_t the_module
    and glutdisplay_func   = L.declare_function "glutDisplayFunc" void_callback_t  the_module
    and glutidle_func      = L.declare_function "glutIdleFunc" void_callback_t  the_module
    and glutsetopt_func    = L.declare_function "glutSetOption" void_int_int_t  the_module
    and glutmainloop_func  = L.declare_function "glutMainLoop" void_void_t  the_module 


    (* NON BOILER PLATE FUNCIONS *)
    and glcolor_func    = L.declare_function "glColor3d"         void_3d_t  the_module
    and glbegin_func    = L.declare_function "glBegin"           void_int_t  the_module
    and glvertex_func   = L.declare_function "glVertex2d"        void_2d_t   the_module
    and glend_func      = L.declare_function "glEnd"             void_void_t the_module
    and glclear_func    = L.declare_function "glClear"           void_int_t the_module 
    and glswap_func      = L.declare_function "glutSwapBuffers"   void_void_t the_module 
    and glutlvmain_func = L.declare_function "glutLeaveMainLoop" void_void_t the_module  
    and glutrepost_func = L.declare_function "glutPostRedisplay" void_void_t the_module

    (* technically not glut *)
    and sin_func = L.declare_function "sin" double_double_t the_module
    and cos_func = L.declare_function "cos" double_double_t the_module
    
    in
    (* END OF GLUT FUNCTION DECLARATIONS *)

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

    (* ADD THE GLUT FUNCTIONS HERE WITH DECLARATION *)
    let glut_decls = 
      let glut_decl m artname fdef  = StringMap.add artname fdef m in
      List.fold_left2 glut_decl StringMap.empty 
      ["setcolor";"vertex";"sin";"cos";"drawpoint"]
      [glcolor_func;glvertex_func; sin_func;cos_func; L.const_int i32_t 0]
    in
    (* No type checking done *)
    let do_glut_func fdef act builder = 
           if glcolor_func    == fdef then   L.build_call glcolor_func     [|act.(0) ; act.(1); act.(2)|] "" builder   
      else if glvertex_func   == fdef then   L.build_call glvertex_func    [|act.(0) ; act.(1) |] "" builder  
      else if sin_func         == fdef then   L.build_call sin_func        [|act.(0)|] "" builder 
      else if cos_func         == fdef then   L.build_call cos_func        [|act.(0)|] "" builder
      (* Draw Point - draws vertices *)
      else L.build_call glvertex_func    [|act.(0) ; act.(1) |] "" builder

    in
    let do_glut_init argc argv title draw_func idle_func builder = 
        let const = L.const_int i32_t
        (* Call all the boilerplate functions *)
        in ignore(L.build_call glutinit_func      [|argc; argv|]            "" builder);
           ignore(L.build_call glutinitdmode_func [|const 2|]               "" builder);
           ignore(L.build_call glutinitwpos_func  [|const 100 ; const 200|] "" builder);
           ignore(L.build_call glutinitwsiz_func  [|const 800 ; const 600|] "" builder);
           ignore(L.build_call glutcreatewin_func [|title|]                 "" builder);
           ignore(L.build_call glutdisplay_func   [| draw_func |]           "" builder);
           ignore(L.build_call glutidle_func      [| idle_func |]           "" builder);
           ignore(L.build_call glutsetopt_func    [|const 0x01F9; const 1|] "" builder);
                  L.build_call glutmainloop_func  [| |]   "" builder

       in 
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
    let memb_index_type sname memb =
      (* Obtain varmap from struct_decls map *)
      let (varmap, _,_) = try StringMap.find sname struct_decls
                    with Not_found -> raise (Failure("Varmap not found for : "^sname^"."^memb))
      in
      (* Obtain index from varmap *)
      try StringMap.find memb varmap
      with Not_found -> raise (Failure("Index not found for : "^sname^"."^memb))
    in
    let memb_index sname memb = snd (memb_index_type sname memb)
    in
    let memb_type sname memb = fst (memb_index_type sname memb)
    in

   (* Fill in the body of the given function *)
   (* This is general as it takes lets user specify function_decls map *)
    let rec _build_function_body fdecl function_decls closure_scopes =
        (* Get the corresponding llvm function value *)
        let (the_function, _) = (match fdecl.A.typ with
                  A.Func -> StringMap.find fdecl.A.fname function_decls
                  | _ -> lookup_method fdecl.A.owner fdecl.A.fname
              ) in

        (* Get an instruction builder that will emit instructions in the current function *)
        let builder = L.builder_at_end context (L.entry_block the_function) in

        (* Checks if function is a draw shape method *)
        let is_draw_shape fdecl = (fdecl.A.typ = A.Method) && (fdecl.A.fname = "draw") &&
            ((let s = List.find (fun s -> s.A.sname = fdecl.A.owner) structs in s.A.ss) = A.ShapeType)
        in
        (* call a closing glend in a shape_draw before before emitting return *)
        let build_custom_ret_void builder =
          ( if is_draw_shape fdecl 
              then ignore(L.build_call glend_func [|  |] "" builder) else ());
          L.build_ret_void builder
        in
        (* For unique globals, use a name that ends with '.' *)
        (* NOTE: there can only be one variable name "foo." that is a unique global *)
        let unique_global n init = match (L.lookup_global n the_module) with
              Some g -> g
            | None -> L.define_global n init the_module
        in
        (* For unique global stringptrs, use a name that ends with '.' *)
        (* NOTE: there can only be one variable name "foo." that is a unique global stringptr *)
        let unique_global_stringptr str n = match (L.lookup_global n the_module) with
              Some g -> L.const_bitcast g i8ptr_t
            | None -> L.build_global_stringptr str n builder
        in
        let clostbl = unique_global "clostbl." (L.const_null i8ptrptr_t)
        in

        (* Format strings for printf call *)
        let int_format_str = unique_global_stringptr "%d" "ifmt."  in
        let char_format_str = unique_global_stringptr "%c" "cfmt."   in
        let string_format_str = unique_global_stringptr "%s" "sfmt." in
        let float_format_str = unique_global_stringptr "%f" "ffmt."  in

(* GLUT RELATED *)
        let time_value  = L.define_global "tv" (L.const_null timeval_struct_t) the_module in
        (* Need to define an actual argc. dummy_arg_1 is now the address of argc *)
        let dummy_arg_1 = L.define_global "argc" (L.const_int i32_t 1) the_module in

        (* The first element of argv *)
        let glut_argv_0  = L.const_bitcast (unique_global_stringptr "ART" "glutstr.") i8ptr_t
        in
        (* Second elment of argv *)
        let glut_argv_1 = (L.const_null i8ptr_t ) in

        (* The argv object itself *)
        let dummy_arg_2 =  L.define_global "glutargv" ( L.const_array i8ptr_t [|glut_argv_0; glut_argv_1|]) the_module
      in
        let g_last_time = unique_global "g_last_time." (L.const_null double_t) in
        let g_delay = unique_global "g_delay." (L.const_null double_t) in
        let g_steps = unique_global "g_steps." (L.const_int i32_t 0) in
        let g_maxiter = unique_global "g_maxiter." (L.const_int i32_t 0) in

(* END OF GLUT RELATED *)
(* Add shape array *)
        let shape_struct =
          let named_struct = L.named_struct_type context "shape_struct."
          in  L.struct_set_body named_struct [| i8ptr_t ; L.pointer_type void_i8p_t |] false ; named_struct
        in
        let shape_list = unique_global "shape_list." (L.const_array shape_struct ( Array.make 100 (L.const_null shape_struct)))
        in
        let shape_list_ind = unique_global "shape_list_ind." (L.const_int i32_t 0)
        in
        let do_seconds_call builder =
          let secptr = ignore(L.build_call  get_tday_func [|time_value ; L.const_null i8ptr_t |] "" builder);
          L.build_gep time_value [|L.const_int i32_t 0; L.const_int i32_t 0 |] "sec" builder in
          let usecptr = L.build_gep time_value [|L.const_int i32_t 0; L.const_int i32_t 1 |] "usec" builder in
          let sec = L.build_sitofp (L.build_load secptr "tmp" builder) double_t "tmp" builder
          and usec = L.build_sitofp (L.build_load usecptr "tmp" builder) double_t "tmp" builder
          in
          let usecisec = L.build_fmul usec (L.const_float double_t 1.0e-6) "tmp" builder in 
                  L.build_fadd sec usecisec "seconds" builder
        in
        let get_unique_draw_func () =
            let get_draw_func = 
                let draw_func = L.define_function "draw." void_void_t the_module in
                let builder = L.builder_at_end context (L.entry_block draw_func) in
                let i = ignore (L.build_call glclear_func     [|L.const_int i32_t 0x4000|] "" builder); 
                        L.build_alloca i32_t "drawi" builder in
                ignore (L.build_store (L.const_int i32_t 0) i  builder);

                let pred_bb = L.append_block context "while" draw_func in
                ignore (L.build_br pred_bb builder); (* builder is in block that contains while stm *)

                (* Body Block *)
                let body_bb = L.append_block context "while_body" draw_func in
                let body_builder = L.builder_at_end context body_bb in 
                let ib = L.build_load i "ib" body_builder in
                (* get the shape *)
                let shape = L.build_load (L.build_gep shape_list [| L.const_int i32_t 0; ib ;L.const_int i32_t 0|] "slp" body_builder) "shp" body_builder in
                (* get the function pointer*)
                let drawshape = L.build_load (L.build_gep shape_list [| L.const_int i32_t 0; ib; L.const_int i32_t 1|] "slfp" body_builder) "drshp" body_builder in
                ignore(L.build_call drawshape [| shape |] "" body_builder);
                (* inrement i *)
                ignore( L.build_store (L.build_add ib (L.const_int i32_t 1) "ib" body_builder) i body_builder);
                ignore(L.build_br pred_bb body_builder);

                let pred_builder = L.builder_at_end context pred_bb in
                let bool_val = 
                  let nmax = L.build_load shape_list_ind "sindex" pred_builder in
                  let ip = L.build_load i "ip" pred_builder in
                  L.build_icmp L.Icmp.Slt ip nmax "tp" pred_builder
                in
                let merge_bb = L.append_block context "merge" draw_func in
                ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
                let merge_builder = L.builder_at_end context merge_bb in
                ignore(L.build_call glswap_func       [||] "" merge_builder);
                ignore (L.build_ret_void merge_builder); draw_func
            in
           (match (L.lookup_function "draw." the_module) with
              Some f -> f
            | None -> get_draw_func)
        in
        let get_idle_func loop_func =
           let idle_stop_condition steps idle_func target_bb builder =
              let bool_val = L.build_icmp  L.Icmp.Sge steps (L.build_load g_maxiter "maxiter" builder) "idlestpcond" builder in
              let merge_bb = L.append_block context "merge" idle_func in (* Merge block *)
              let merge_builder = L.builder_at_end context merge_bb in
              let then_bb = L.append_block context "then" idle_func in
              let then_builder = L.builder_at_end context then_bb in
              ignore (L.build_cond_br bool_val then_bb merge_bb builder);
              ignore (L.build_call glutlvmain_func  [||] "" then_builder);
              ignore (L.build_br merge_bb then_builder);       
              ignore (L.build_call glutrepost_func  [||] "" merge_builder);
              ignore (L.build_br target_bb merge_builder);
              merge_bb



           in
           let idle_func = L.define_function "idle." void_void_t the_module in
           let builder = L.builder_at_end context (L.entry_block idle_func) in
           let current_time = L.build_alloca double_t "currtime" builder in
           let currtval = do_seconds_call builder in
            ignore (L.build_store currtval current_time  builder);
           let lastt = L.build_load g_last_time "lastt" builder in
           let delay = L.build_load g_delay "delay" builder in
           let lag = L.build_fsub currtval lastt "rdelay" builder in
           let bool_val = L.build_fcmp L.Fcmp.Oge lag delay "ifcond" builder 
           in
           let merge_bb = L.append_block context "merge" idle_func in (* Merge block *)
           let merge_builder = L.builder_at_end context merge_bb in
           let then_bb = L.append_block context "then" idle_func in
           let then_builder = L.builder_at_end context then_bb in
           let stepinc = L.build_fptoui (L.build_fdiv lag delay "finc" then_builder) i32_t "stepinc" then_builder in
           let newsteps = L.build_add (L.build_load g_steps "steps" then_builder) stepinc "newsteps" then_builder in
           ignore(L.build_store newsteps g_steps then_builder);
           ignore (L.build_call loop_func [| |] "" then_builder);
           ignore (L.build_store currtval g_last_time then_builder);
           ignore (idle_stop_condition newsteps idle_func merge_bb then_builder);

           (* builder is in block that contains if stmt *)
           ignore (L.build_cond_br bool_val then_bb merge_bb builder);
           ignore (L.build_ret_void merge_builder); idle_func

        in
        (* closure related functions *)
        (* get list of name references to variables that are not block local 
           i.e variables referenced that are neithe in local_decls or in scopes *)
        let rec non_block_locals local_decls stmt_list scopes =
          let scopes =
            let locals = List.fold_left (fun m (t, n,_) -> StringMap.add n (L.const_int i32_t 0,t) m ) StringMap.empty local_decls
            in (locals, LocalScope)::scopes
          in
          let rec scope_iter n scopes =
            let hd = List.hd scopes in 
                (match hd with
                    (globs, GlobalScope) -> fst(StringMap.find n globs)
                  | (locls, LocalScope)  -> ( try fst(StringMap.find n locls)
                                              with Not_found -> scope_iter n (List.tl scopes) )
                  | (_, StructScope) -> raise (Failure ("No struct scope in closure"))
                  | (_, ClosureScope)  -> raise (Failure ("No nested closure"))
                )
          in
          let rec non_local_expr = function
              A.Id s -> (try ignore(scope_iter s scopes); StringSet.empty with Not_found -> StringSet.singleton s)
            | A.Vecexpr(e1,e2) -> StringSet.union (non_local_expr e1)  (non_local_expr e2)
            | A.Binop(e1,_,e2) -> StringSet.union (non_local_expr e1)  (non_local_expr e2)
            | A.Asnop(e1,_,e2) -> StringSet.union (non_local_expr e1)  (non_local_expr e2)
            | A.Unop(_,e)      -> non_local_expr e
            | A.Posop(_,e)     -> non_local_expr e
            | A.Trop(_,e1,e2,e3) -> StringSet.union (non_local_expr e1) (StringSet.union (non_local_expr e2)  (non_local_expr e3))
            | A.Call(e1,el)  -> StringSet.union (match e1 with A.Id s-> StringSet.empty | _ -> non_local_expr e1) 
                                 ( List.fold_left (fun set e-> StringSet.union set (non_local_expr e)) StringSet.empty el)
            | A.Index(e1, e2)-> StringSet.union (non_local_expr e1)  (non_local_expr e2)
            | A.Member(e, _) -> non_local_expr e
            | _  -> StringSet.empty
          in
          let rec non_local = function
              A.Block(decls, stmts,_) -> non_block_locals decls stmts scopes
            | A.Expr(e)   -> non_local_expr e
            | A.Return(e) -> non_local_expr e
            | A.If(e,s1,s2) -> StringSet.union (non_local_expr e) (StringSet.union (non_local s1)  (non_local s2))
            | A.For(e1,e2,e3,s) -> StringSet.union (StringSet.union (non_local_expr e1) (non_local_expr e2)) 
                                    (StringSet.union (non_local_expr e3)  (non_local s))
            | A.ForDec(decls,e2,e3,s)-> non_local ( A.Block(decls, [A.For(A.Noexpr, e2, e3, s)],A.PointContext))
            | A.While(e,s) -> StringSet.union (non_local_expr e) (non_local s)
            | _ -> StringSet.empty
            (*| A.Timeloop of string * expr * string * expr * stmt
            | A.Frameloop of string * expr * string * expr * stmt *) (* No nested timelops *)
          in
          List.fold_left (fun set s-> StringSet.union set (non_local s)) StringSet.empty stmt_list
              
        in
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

        (* Construct the function's formal arguments. Allocate each on the stack, initialize their
           value,  and remember their values in the "formals" map *)
        (* NOTE: the functions top level local vars are constructed in the build_block_body. This means formal vars (params)
          are check after top level local var during lookup, even though they are semantically at the same level. While local hiding
          formal is technically possible it is prohibited during the semantic check stage. *)
        let formal_vars =
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

          (* llvm type list for the params *)
          let lparams = (match fdecl.A.typ with
                    A.Func -> Array.to_list (L.params the_function)
                  (* For Method/Const drop the "this" param as it needs to be inaccessible to user *)
                  | _ -> List.tl (Array.to_list (L.params the_function)))
          in
          List.fold_left2 add_formal StringMap.empty fdecl.A.params lparams

        in
          
        (* Invoke "f builder" if the current block doesn't already
            have a terminal (e.g., a branch). *)
        let add_terminal builder f =
            match L.block_terminator (L.insertion_block builder) with
                Some _ -> ()
              | None -> ignore (f builder) 
        in

        (* Build the body of a code execution block and return builder *)
        let rec build_block_body (local_decls, stmt_list) builder scopes =

            (* Prepend the block local variables to the scopes list *)
            (* Prepend any initializers to the statment list *)
            let (stmt_list, scopes) = 
                let add_local m (t,n) =
                  (* Currently all block local variables are allocated at the entry block 
                     This prevents multiple allocas for loop local variables. The only issue with this
                     method is that in the output file, variables are allocated in reverse order *)
                     (* NOTE this translation should be moved to the semantic part of the code *)
                  let builder = L.builder_at context (L.instr_begin(L.entry_block the_function)) in
                  let local_var = L.build_alloca (ltype_of_typ t) n builder (* allocate space for local *)
                  in StringMap.add n (local_var,t) m in
                let (stmt_list, local_decls) = List.fold_left
                      (* Handle expression initers by adding them as assignment expression statments *)
                      (fun (sl, ld) (t,n,i) -> 
                            ( match i with A.Exprinit e -> A.Expr( A.Asnop(A.Id(n),A.Asn, e) )::sl , (t,n)::ld  
                              | _  -> sl, (t,n)::ld (* Silently ingore NoInit and ListInit *) 
                            )
                      ) (stmt_list, [])
                    (* Need to reverse since we are pushing to top of stmt_list. Luckily, the fold unreversed local_decls *)
                    (List.rev local_decls)
                in
                let locals =  List.fold_left add_local StringMap.empty local_decls
                in stmt_list,(locals, LocalScope)::scopes
            in

            let string_create s builder =
              let str = L.build_global_stringptr s "temp" builder in
              L.build_in_bounds_gep str [|L.const_int i32_t 0|] "temps" builder
            in
            (* Return the value for a variable by going through the scope chain *)
            let rec _lookup n builder scopes =
                let succ ci =(*successor of const_int i32_t in int*)
                      let int_of_const ci = ( match (L.int64_of_const ci) with
                              Some i -> Int64.to_int(i)
                            | None -> 0 )
                     in (int_of_const ci) + 1
                in
                let hd = List.hd scopes in
                (match hd with
                    (globs, GlobalScope) -> fst(StringMap.find n globs)
                  | (locls, LocalScope)  -> ( try fst(StringMap.find n locls)
                                              with Not_found -> _lookup n builder (List.tl scopes) )
                  | (_, StructScope) -> ( try (
                    let ind = try memb_index fdecl.A.owner n with Failure s -> raise Not_found in
                      (* If member, get its pointer by dereferencing the first argument
                        corresponding to the "this" pointer *)
                      let e' = L.param (fst (lookup_function fdecl.A.fname) ) 0  in
                      L.build_gep e' [|L.const_int i32_t 0; L.const_int i32_t ind |] "tmp" builder
                    ) with Not_found -> _lookup n builder (List.tl scopes) )
                  | (locls, ClosureScope) -> try 
                                    ( let (i, t) = StringMap.find n locls in
                                      let clostbl = L.build_bitcast (L.build_load clostbl "clostblptr" builder) (L.pointer_type(L.array_type i8ptr_t (succ i))) "clostblarr" builder in
                                      let ppt = L.build_load (L.build_gep clostbl [| L.const_int i32_t 0; i|] "valppt" builder) "valppt" builder in                  
                                      L.build_bitcast ppt  (L.pointer_type (ltype_of_typ t)) "valpt" builder
                                    ) with Not_found -> _lookup n builder (List.tl scopes) 
                )
            in

            (* Return the type for a variable by going through the scope chain *)
            let rec _lookup_type n scopes =
                let hd = List.hd scopes in
                (match hd with
                    (globs, GlobalScope) -> snd(StringMap.find n globs)
                  | (locls, LocalScope)  -> ( try snd(StringMap.find n locls)
                                              with Not_found -> _lookup_type n (List.tl scopes) )
                  | (locls, ClosureScope) -> ( try snd(StringMap.find n locls)
                                              with Not_found -> _lookup_type n (List.tl scopes) )
                  | (_, StructScope) -> try memb_type fdecl.A.owner n
                                        with Failure s -> _lookup_type n (List.tl scopes)
                )
            in
            let lookup n builder = _lookup n builder scopes 
            in
            let lookup_type n = _lookup_type n scopes 
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
            | A.FloatLit f -> ("double", A.Float)
            | A.Id s -> let t =  lookup_type s in (string_of_typ2 t, t)
            | A.VecLit(f1, f2) -> ("vec", A.Vec)
            | A.Vecexpr(e1, e2) -> ("vec", A.Vec)
            | A.Index(a, e) -> (match snd(expr_type a) with (* First get type of the expr being indexed *)
                                (* The type of the index expression is the subtype 't' of the array *)
                                A.Array (t, e2) -> (string_of_typ2 t, t)
                              | A.Vec -> (string_of_typ2 A.Float, A.Float)
                              | _ -> raise (Failure ("Indexing non array")))

            | A.Member(e, s) -> let (n,t) = expr_type e (* Get type name of the expression before dot *)
                          (* Look for it in the structs map and get the var map *)
                          in  let (varmap, _,_ ) = StringMap.find n struct_decls
                          (* Look for string after the dot in the varmap *)
                          in let t = fst(StringMap.find s varmap)
                          in (string_of_typ2 t, t)
            | A.StringLit s -> ("string",A.String)
            |e -> ("Void",A.Void)(*raise (Failure ("Unsupported Expression for expr_type"^A.string_of_expr e))*)

            in


            let convert_type e1 e2 builder =
                let float_type = L.type_of (L.const_float double_t 1.1) and int_type = L.type_of(L.const_int i32_t 1) and type_of_e1=L.type_of(e1) and type_of_e2=L.type_of(e2)
                  in let e1' =
                          (if type_of_e1=int_type
                              then L.build_sitofp e1 float_type "temp" builder
                            else e1)
                  and e2'=(if type_of_e2=int_type
                            then L.build_sitofp e2 float_type "temp" builder 
                           else e2 )
                in (e1',e2')

              in


            
            let match_type typ op =
                  let float_type = (L.type_of (L.const_float double_t 1.1))
                  and vec_type = L.type_of(L.const_vector [|L.const_float double_t 1.1 ; L.const_float double_t 1.1 |])
                in 
                if typ=vec_type
                  then match op with
                      A.Add -> L.build_fadd
                    | A.Sub     -> L.build_fsub
                    | A.Mult    -> L.build_fmul
                    | A.Div     -> L.build_fdiv
                    | _         -> raise (Failure "Unsupported binary operation for vectors")

                else if typ=float_type
                  then match op with
                    A.Add -> L.build_fadd
                    | A.Sub     -> L.build_fsub
                    | A.Mult    -> L.build_fmul
                    | A.Div     -> L.build_fdiv
                    | A.And     -> L.build_and
                    | A.Or      -> L.build_or
                    | A.Mod     -> raise (Failure "Cannot mod a float")
                    (* Need to think about these ops *)
                    | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
                    | A.Neq     -> L.build_fcmp L.Fcmp.One
                    | A.Less    -> L.build_fcmp L.Fcmp.Olt
                    | A.Leq     -> L.build_fcmp L.Fcmp.Ole
                    | A.Greater -> L.build_fcmp L.Fcmp.Ogt
                    | A.Geq     -> L.build_fcmp L.Fcmp.Oge
                    
                else match op with
                    A.Add -> L.build_add
                    | A.Sub     -> L.build_sub
                    | A.Mult    -> L.build_mul
                    | A.Div     -> L.build_sdiv
                    | A.And     -> L.build_and
                    | A.Or      -> L.build_or
                    | A.Mod     -> L.build_srem
                    | A.Equal   -> L.build_icmp L.Icmp.Eq
                    | A.Neq     -> L.build_icmp L.Icmp.Ne
                    | A.Less    -> L.build_icmp L.Icmp.Slt
                    | A.Leq     -> L.build_icmp L.Icmp.Sle
                    | A.Greater -> L.build_icmp L.Icmp.Sgt
                    | A.Geq     -> L.build_icmp L.Icmp.Sge
              
            in
            let vec_scalar_mult e1'' e2'' builder =
                let type_of_e1'' = L.type_of(e1'') 
                and type_of_e2'' = L.type_of(e2'')
                and float_type = L.type_of(L.const_float double_t 1.1)
                and vec_type = L.type_of(L.const_vector [|L.const_float double_t 1.1 ; L.const_float double_t 1.1 |])
                in
                (
                  if type_of_e2'' = float_type
                  then 
                  (
                    let vec_of_e2'' = L.const_vector [| L.const_float double_t 0.0 ; L.const_float double_t 0.0 |]
                  in
                    let insert_element1 = L.build_insertelement vec_of_e2'' e2'' (L.const_int i32_t 0) "tmp1" builder
                  in
                    let insert_element2 = L.build_insertelement insert_element1 e2'' (L.const_int i32_t 1) "tmp2" builder
                  in
                    match_type vec_type A.Mult e1'' insert_element2 "tmp" builder
                  )
                  else 
                  (
                    if type_of_e1'' = float_type
                    then
                    ( 
                      let vec_of_e1'' = L.const_vector [| L.const_float double_t 0.0 ; L.const_float double_t 0.0 |]
                    in
                      let insert_element1 = L.build_insertelement vec_of_e1'' e1'' (L.const_int i32_t 0) "tmp1" builder
                    in
                      let insert_element2 = L.build_insertelement insert_element1 e1'' (L.const_int i32_t 1) "tmp2" builder
                    in
                      match_type vec_type A.Mult insert_element2 e2'' "tmp" builder
                    )
                    else
                      raise (Failure "Unsupported binary operation for vectors")
                  )
                )
            in

            (* Construct code for an lvalue; return a pointer to access object *)
            let rec lexpr builder = function
                A.Id s -> lookup s builder
              | A.Index(e1,e2) -> let e2' = expr builder e2 in
                  ( match snd(expr_type e1) with 
                      A.Array(_,_) -> L.build_gep (lexpr builder e1) [|L.const_int i32_t 0; e2'|] "tmp" builder
                    | _ -> let e1' = expr builder e1 in (* e1 should be indexible *)
                           let tmp = L.build_alloca (L.type_of e1') "indtmp" builder in
                           ignore (L.build_store e1' tmp builder);
                      L.build_gep tmp [|L.const_int i32_t 0; e2'|] "tmp" builder
                  )
              | A.Member(e, s) -> let e' = lexpr builder e in
                  (* Obtain index of s in the struct type of expression e *)
                  let (sname, _ ) = expr_type e in let i = memb_index sname s in
                  L.build_gep e' [|L.const_int i32_t 0; L.const_int i32_t i|] "tmp" builder
              | e -> raise (Failure ("Trying to assign to an non l-value: "^(A.string_of_expr e)))

    
            (* Construct code for an expression; return its value *)
            and expr builder = function (* Takes args builder and Ast.expr *)
                A.IntLit i -> L.const_int i32_t i
              | A.CharLit c -> L.const_int i8_t (int_of_char c) (* 1 byte characters *)
              | A.Noexpr -> L.const_int i32_t 0  (* No expression is 0 *)
              | A.StringLit s -> string_create s builder
              | A.FloatLit f -> L.const_float double_t f
              | A.VecLit(f1, f2) -> L.const_vector [|(L.const_float double_t f1) ; (L.const_float double_t f2)|]
              | A.Vecexpr(e1, e2) -> 
                  let e1' = expr builder e1
                  and e2' = expr builder e2
                in
                  let tmp_vec = L.const_vector [| L.const_float double_t 0.0 ; L.const_float double_t 0.0 |]
                in
                  let insert_element1 = L.build_insertelement tmp_vec e1' (L.const_int i32_t 0) "tmp1" builder
                in
                  L.build_insertelement insert_element1 e2' (L.const_int i32_t 1) "tmp2" builder
              | A.Id s -> L.build_load (lookup s builder) s builder (* Load the variable into register and return register *)
              | A.Binop (e1, op, e2) ->
                  let e1' = expr builder e1 
                  and e2' = expr builder e2 
                  and float_type = L.type_of(L.const_float double_t 1.1)
                  and vec_type = L.type_of(L.const_vector [|L.const_float double_t 1.1 ; L.const_float double_t 1.1 |])
                in
                  let type_of_e1' = L.type_of(e1') and type_of_e2' = L.type_of(e2')
                in 
                if type_of_e1' <> type_of_e2'
                    then let ret= convert_type e1' e2' builder
                        in let x = fst ret and y = snd ret
                        in 
                        (if ((L.type_of x) = vec_type || (L.type_of y) = vec_type) && op = A.Mult
                          then vec_scalar_mult x y builder (*Handle vector scalar multiplication *)
                          else match_type float_type op x y "temp" builder
                        )
                    else
                      match_type type_of_e1' op e1' e2' "tmp" builder

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
                    let leftyp1=(L.type_of (L.const_int i32_t 1)) and leftyp2 = (L.type_of (L.const_float double_t 1.1))
                     and leftyp3=(L.type_of e') in
                  (match op with
                    A.Neg     -> (if leftyp1 = leftyp3 then (L.build_neg) else (L.build_fneg))
                  | A.Not     -> L.build_not
                  | _  -> raise (Failure "Unsupported unary op")(* Ignore other unary ops *)
                    ) e' "tmp" builder


                (* This ok only for few built_in functions *)
              | A.Call (A.Id "printi", [e]) -> L.build_call printf_func [|int_format_str ; (expr builder e) |] "printf" builder
              | A.Call (A.Id "printc", [e]) -> L.build_call printf_func [|char_format_str ; (expr builder e) |] "printf" builder
              | A.Call (A.Id "prints", [e]) -> L.build_call printf_func [|string_format_str ; (expr builder e) |] "printf" builder
              | A.Call (A.Id "printf", [e]) -> L.build_call printf_func [|float_format_str ; (expr builder e) |] "printf" builder
              | A.Call (A.Id "addshape", el) -> 
                    let add_one_shape ex = 
                      let fdef' = L.const_bitcast (fst(lookup_method (fst(expr_type ex)) "draw")) (L.pointer_type void_i8p_t) in
                      let i = L.build_load shape_list_ind "sindex" builder in
                      let ex' = L.build_bitcast (lexpr builder ex) i8ptr_t "shp" builder in
                      (* store the shape *)
                      ignore( L.build_store ex' (L.build_gep shape_list [| L.const_int i32_t 0; i; L.const_int i32_t 0|] "slp" builder) builder);
                      (* store the function *)
                      ignore( L.build_store fdef' (L.build_gep shape_list [| L.const_int i32_t 0; i; L.const_int i32_t 1|] "slfp" builder) builder);
                      (* increment i *)
                      ignore( L.build_store (L.build_add i (L.const_int i32_t 1) "tmp" builder) shape_list_ind builder); ()
                    in ignore(List.iter add_one_shape el); L.undef void_t
                (* A call without a dot expression refers to three possiblities. In order of precedence: *)
                (* constructor call, method call (within struct scope), function call *)
              | A.Call (A.Id f, act) ->
                 (* The llvm type array of the calling functions parameters
                    Can be use to retreive the "this" argument *)
                 (try let fdef = StringMap.find f glut_decls in 
                  let actuals = if (f = "drawpoint") (* Convert vector into two arguments *)
                    then let v = (List.hd act) in List.map (expr builder) [ A.Index(v,A.IntLit(0)) ; A.Index(v,A.IntLit(1))]
                    else List.rev (List.map (expr builder) (List.rev act)) in
                  do_glut_func fdef (Array.of_list actuals) builder
                with Not_found -> (
                
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
               ))
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

            (* Build the code for the given statement; return the builder for
             the statement's successor *)
            let rec stmt builder = function
                  A.Block (vl, sl,ctxt) ->
                    (* Handle context start *)
                    let glcontext = function A.PointContext -> 0 | A.LineContext -> 1 | A.TriangleContext -> 4 in
                    let contexti = L.const_int i32_t (glcontext ctxt) in

                    let builder =
                    (* start a new context if necessary.*)
                    (if ctxt = A.PointContext then () (* Point context is noop *)
                      else (ignore(L.build_call glend_func [| |] "" builder);
                            ignore(L.build_call glbegin_func [|contexti|] "" builder))
                    );
                    build_block_body (vl, sl) builder scopes
                    in
                    (if ctxt = A.PointContext then () (* Point context is noop *)
                      else (ignore(L.build_call glend_func [| |] "" builder);
                            ignore(L.build_call glbegin_func [|L.const_int i32_t 0|] "" builder))
                    );
                    builder
                | A.Expr e -> ignore (expr builder e); builder  (* Simply evaluate expression *)

                | A.Return e -> ignore (match fdecl.A.rettyp with  (* Different cases for void and non-void *)
                    A.Void -> build_custom_ret_void builder
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
                    ( A.Block ( [], [  A.Expr e1; A.While (e2, A.Block ([], [body; A.Expr e3],A.PointContext) ) ] , A.PointContext) )
                | A.ForDec (vdecls, e2, e3, body) -> stmt builder ( A.Block(vdecls, [A.For(A.Noexpr , e2, e3, body)], A.PointContext) )
                | A.Timeloop(s1, e1, s2, e2, stmt) ->
                    let ftype = L.function_type void_t [| |] in
                    let fdef = L.define_function "timeloop." ftype the_module in
                    let loopdecl = {A.rettyp = A.Void ; A.fname = "timeloop." ; A.params = [];
                                    A.locals = [(A.Float, s1,A.Exprinit(e1)) ; (A.Float, s2, A.Exprinit(e2))];
                                    A.body = [stmt]; A.typ = A.Func ; A.owner=""} in
                    let fdecls = StringMap.add "timeloop." (fdef, loopdecl) function_decls in
                    let outnames = StringSet.elements ( non_block_locals loopdecl.A.locals 
                                  loopdecl.A.body [(global_vars, GlobalScope)] )
                    in  
                    let table = (*ignore(prerr_endline("[" ^ String.concat ", " outnames ^"]"));*)
                                L.build_array_malloc i8ptr_t (L.const_int i32_t (List.length outnames)) "ctable" builder
                    in
                    let tablearr = L.build_bitcast table (L.pointer_type(L.array_type i8ptr_t (List.length outnames))) "ctablearr" builder in
                    let clostblptr =  L.build_bitcast table i8ptrptr_t "toclostbl" builder

                    in (* Closure build and teardown is expensive *)
                    let (closure_map, _) = ignore(L.build_store clostblptr clostbl builder); 
                      let add_to_closure (m,i) n = 
                        let valpointer = L.build_bitcast (lookup n builder) i8ptr_t "valp" builder in
                        let pospointer = L.build_gep tablearr [| L.const_int i32_t 0; L.const_int i32_t i|] "valp" builder 
                        in ignore(L.build_store valpointer pospointer builder);
                        (StringMap.add n (L.const_int i32_t i, lookup_type n) m , i+1)
                      in List.fold_left add_to_closure (StringMap.empty, 0) outnames
                    in
                    ignore( _build_function_body loopdecl fdecls [(closure_map, ClosureScope)]);
                    (* Setup the timer and stepper values *)
                    ignore(L.build_store (do_seconds_call builder) g_last_time builder);
                    let e1' = expr builder e1 in let e2' = expr builder e2 in
                    ignore(L.build_store e1' g_delay builder);
                    ignore(L.build_store (L.const_int i32_t 0) g_steps builder);
                    let delayflt = L.build_fdiv e2' e1' "stepsflt" builder in
                    ignore(L.build_store (L.build_fptoui delayflt i32_t "stepsflt" builder) g_maxiter builder);
                    ignore(do_glut_init dummy_arg_1 (L.const_bitcast dummy_arg_2 i8ptrptr_t) glut_argv_0 (get_unique_draw_func())  (get_idle_func fdef)builder);
                    ignore(L.build_free table builder); 
                    ignore(L.build_store (L.const_int i32_t 0) shape_list_ind builder);
                    builder
                | t -> raise (Failure ("Unsupported statement type "^A.string_of_stmt t))(* Ignore other statement type *)
            in
            (* Build the code for each statement in the block 
              and return the builder *)
            List.fold_left stmt builder stmt_list
        in 
        (* End of build_block_body *)
        (* Construct the scopes list before calling build_block_body *)
        let scopes_list = match fdecl.A.typ with
                    (* Functions can't access members so no struct scope *)
                    A.Func -> closure_scopes@[(formal_vars, LocalScope); (global_vars, GlobalScope) ]
                    (* Don't need  struct scope map as we have one and the type doesn't match as well.
                       So we are using an empty map *)
                  | _      -> closure_scopes@[(formal_vars, LocalScope); (StringMap.empty, StructScope) ; (global_vars, GlobalScope)]
        in 

        let builder = 
          (* Add glbegin to beginning of shape draw methods *)
          ( if (is_draw_shape fdecl) 
              then ignore(L.build_call glbegin_func     [|L.const_int i32_t 0 |] "" builder)
              else ()
          );
          build_block_body (fdecl.A.locals, fdecl.A.body) builder scopes_list in
        (* Add a return if the last block falls off the end *)
        add_terminal builder (match fdecl.A.rettyp with
            A.Void -> build_custom_ret_void
          | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
    in
    (* old build_function_body *)
    let build_function_body fdecl = _build_function_body fdecl function_decls []
    in

    List.iter build_function_body functions;
    (* Build methods and constructors for each struct *)
    List.iter (fun sdecl -> List.iter build_function_body (sdecl.A.ctor::sdecl.A.methods)) structs;
    the_module