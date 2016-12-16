open Ast

module StringMap = Map.Make(String)
type scope = GlobalScope | LocalScope  | StructScope
(* Semantic checking of a program. Returns possibly modified Ast if successful,
   throws an exception if something is wrong. *)
let report_dup  exceptf list =
        let rec helper = function 
            n1 :: n2 ::_ when n1=n2 -> raise (Failure(exceptf n1) )
            |_ :: t -> helper t
            |[]->()

        in helper(List.sort compare list)
(* lftype, rtype, errmsg *)
let check_ass lval rval err =
    if lval = rval then lval else 
    (match lval with 
        UserType(s,ShapeType) when s=".shape" ->rval (*Since addshape demands a string  for the name that it will use as a type I give it a String Dummy and will past semant*)
        |_-> raise err                        (*Since this is a special case I supsend the one to one checking and just use rval*)
    )


let struct_build prog =
    let globals = prog.v
    and functions = prog.f

    in  
    (* A map of all struct/shape types *)
    let structs = List.fold_left ( fun m st -> StringMap.add st.sname st m)
                StringMap.empty prog.s in
    let (structs,funcs) = (* Refers to structs and non-member functions *)
        (* Puts methods and constructors with appropriate struct and returns tuple
            (map, bool) *)
        let filter_func m f =
          match f.typ with
            Func -> (m, true) (* true means keep function *)
          | Constructor -> let s = try StringMap.find f.owner m
                    with Not_found -> raise (Failure ("constructor of undefined struct: " ^ f.owner^"::"^f.fname))
                in if (s.ctor.fname="") then (StringMap.add s.sname
                            {ss = s.ss;sname = s.sname; decls = s.decls; ctor = f; methods = s.methods} m , false)
                        else
                            raise(Failure("There already exists a constructor called " ^f.fname ^" in struct " ^s.sname))
          | Method -> let s = try StringMap.find f.owner m
                    with Not_found -> raise (Failure ("method of undefined struct: " ^ f.owner^"::"^f.fname))
                in try ignore( List.find (fun f2 -> f2.fname = f.fname) s.methods);
                       raise(Failure("There already exists a method called " ^f.fname ^" in struct " ^s.sname))   
                    with Not_found -> (StringMap.add s.sname
                                {ss = s.ss;sname = s.sname; decls = s.decls; ctor = s.ctor; methods = f::s.methods} m , false)
        in
        List.fold_left ( fun (m,l) f -> let (m, cond) = filter_func m f in
        if cond then (m, f::l) else (m, l) ) (structs, []) functions
    in
    { s = List.map (fun st -> let s = StringMap.find st.sname structs in
            (* If no contructor is defined add default *)
            {ss = s.ss;sname = s.sname; decls = s.decls; ctor = if (s.ctor.fname="") then default_ctr s.sname else s.ctor; methods =s.methods}
        ) prog.s;
      f = List.rev funcs ; v = globals }

let check prog =
    (* Get the global variables and functions *)
    let globals = prog.v
    and functions = prog.f
    and structs = prog.s
    in
    
        report_dup(fun n-> "Duplicate Function Name " ^n)(List.map (fun fd -> fd.fname)functions); (*Does pretty basic superfical checking if there exists duplicate function names, global or structs*)

        report_dup(fun n-> "Duplicate Global Name " ^n)(List.map (fun (_,a,_) ->  a)globals);

        report_dup(fun n-> "Duplicate Struct Name " ^n)(List.map(fun st -> st.sname)structs); 

let built_in_fun = StringMap.add "printi"
{rettyp=Void; fname="printi";params=[(Int, "x",Value)];locals=[];body=[];typ=Func;owner="None"}
(StringMap.add"printf" {rettyp=Void; fname="printf";params=[(Float, "x",Value)];locals=[];body=[];typ=Func;owner="None";}       (*Builds a list of the predefined functions special note about add shape it demands a name to contstruct the type *)
(StringMap.add "prints" {rettyp=Void; fname="printf";params=[(String, "x",Value)];locals=[];body=[];typ=Func;owner="None";}
(StringMap.add "addshape"{rettyp=Void;fname="addshape";params=[(UserType(".shape",ShapeType),"x",Value)];locals=[];body=[];typ=Func;owner="None";}
(StringMap.add "cos"{rettyp=Float;fname="cos";params=[(Float,"x",Value)];locals=[];body=[];typ=Func;owner="None";}
(StringMap.add "sin"{rettyp=Float;fname="sin";params=[(Float,"x",Value)];locals=[];body=[];typ=Func;owner="None";}
(StringMap.add "setcolor"{rettyp=Void;fname="setcolor";params=[(Float,"x",Value);(Float,"x",Value);(Float,"x",Value)];locals=[];body=[];typ=Func;owner="None";}
(StringMap.add "drawpoint"{rettyp=Void;fname="drawpoint";params=[(Float,"x",Value);(Float,"x",Value)];locals=[];body=[];typ=Func;owner="None";}
(StringMap.singleton "printc" {rettyp=Void; fname="printf";params=[(Char, "x",Value)];locals=[];body=[];typ=Func;owner="None";}))))))))
(*let function_decls =
    List.map(fun fd -> fd.fname) functions*)
in 
let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                        built_in_fun functions

in
let function_decl s = try StringMap.find s function_decls       (*Builds a string map of name of func to function recored*)
    with Not_found -> raise (Failure ("Unrecognized function " ^ s ^" did you forget to define it ?"))
in

let _ = function_decl "main" (*Makes sure that main is defined*)
             
in

let global_vars = List.fold_left(fun m(t,n,_)->StringMap.add n t m) StringMap.empty(globals)
in
let function_check func =

    let formal_vars = List.fold_left(fun m(t,n,_)->StringMap.add n t m) StringMap.empty (func.params)
    in
    (* Top level local_vars *)
    let local_vars =  List.fold_left(fun m(t,n,i)-> StringMap.add n t m)
                            formal_vars (func.locals)
    in
        report_dup(fun n-> "Duplicate Parameter Name " ^n ^"in " ^ func.fname)(List.map (fun (_,a,_) ->  a)func.params);    (*Checks is there exists duplicate parameter names and function local name*)
        report_dup(fun n-> "Duplicate local Name " ^n ^ " in " ^ func.fname)
            ((List.map (fun (_,a,_) ->  a)func.params)@(List.map (fun (_,a,_) ->  a)func.locals));  

    (* Map of struct name to struct *)
    let struct_name_list = List.fold_left(fun m usr -> StringMap.add usr.sname usr m) (*creates a struct names list*)
        StringMap.empty(structs)
    in 
    (* Given struct give map of method names to method *)
    let get_member_funcs name = let st = try StringMap.find name struct_name_list (*creates a struct member funciontlist*)
        with  Not_found -> raise(Failure("Could not find memeber func"))
        in List.fold_left(fun m s -> StringMap.add s.fname s m)
            StringMap.empty st.methods
    in 
    (* Get constructor *)
    let get_member_constr name = let st = StringMap.find name struct_name_list (*creates a struct member funcion list*)
        in st.ctor
    in
    
    (* Get map of memb_variables to their type *)
    let get_struct_member_var name = let st = try StringMap.find name struct_name_list
        with  Not_found -> raise(Failure("Could not find member func"))
        in List.fold_left(fun m (t,n) -> StringMap.add n t m)
            StringMap.empty st.decls
    in
    
    (* Gets type of var in struct *)
    let member_var_type name var =
        let temp = get_struct_member_var name
        in
        StringMap.find var temp   
    in       
    
    (* Gets mem_func *)
    let get_mem_func_name name func =
        let temp = get_member_funcs name 
    in try StringMap.find func temp
            with Not_found -> raise(Failure(func ^ " is not a member function of " ^name))
    in
    
    let lookup_function f =
      (* First try to find a matching constructor *)
      try get_member_constr f
      (* If that fails try to find a method.
         this is guaranteed to fail in a normal function *)
      with Not_found -> (try get_mem_func_name func.owner f
      (* Finally look for normal function *)
            with Not_found | Failure _ -> StringMap.find f function_decls)
    in
    let rec check_block(local_decls, stmt_list) scopes =

        (* Prepend the block local variables to the scopes list *)
        (* Prepend any initializers to the statment list *)
        let (stmt_list, scopes) = 
            report_dup(fun n-> "Duplicate local Name " ^n ^ " in " ^ func.fname)(List.map (fun (_,a,_) ->  a)local_decls);
            let add_local m (t,n) =  StringMap.add n t m in
                 (* NOTE this translation should be moved to the semantic part of the code *)
            let (stmt_list, local_decls) = List.fold_left
                  (* Handle expression initers by adding them as assignment expression statments *)
                  (fun (sl, ld) (t,n,i) -> 
                        ( match i with Exprinit e ->  Expr( Asnop(Id(n),Asn, e) )::sl , (t,n)::ld  
                          | _  ->  sl, (t,n)::ld (* Silently ingore NoInit and ListInit. HANDLE OTHER INTIIALIZERS*) 
                        )
                  ) (stmt_list, [])
                (* Need to reverse since we are pushing to top of stmt_list. Luckily, the fold unreversed local_decls *)
                (List.rev local_decls)
            in
            let locals =  List.fold_left add_local StringMap.empty local_decls
            in  stmt_list,(locals, LocalScope)::scopes
        in
        (* Recursive ret_type *)
        let rec _ret_type n scopes =
            let hd = List.hd scopes in
                try (match hd with
                    (globs, GlobalScope) -> StringMap.find n globs
                  | (locls, LocalScope)  -> ( try StringMap.find n locls
                                              with Not_found -> _ret_type n (List.tl scopes) )
                  | (_, StructScope) -> (
                    try member_var_type func.owner n with Failure _ -> _ret_type n (List.tl scopes) )
                )
                with Not_found -> raise(Failure("Undeclared variable " ^n))
        in
        (* Gets type for variable name s [old ret_type]*)
        let ret_type n = _ret_type n scopes 
        in
        (* Gets type of expression. [LATER ON HANDLE WITH SAST] *)
        let rec expr_b = function
             IntLit _-> Int
            |CharLit _-> Char
            |StringLit _-> String
            |FloatLit _ -> Float
            |VecLit (_,_)-> Vec
            |Id s -> ret_type s
            |Binop(e1,op,e2) -> let e1' = expr_b e1 and e2'=expr_b e2 in
            (match op with
                Add|Sub|Mult|Div|Mod when e1'=Int && e2'=Int -> Int
                |Add|Sub|Mult|Div when e1'=Vec&&e2'=Vec -> Vec
                |Mult when e1'=Vec&&e2'=Int -> Vec
                |Mult when e1'=Vec&&e2'=Float -> Vec
                |Mult when e1'=Int&&e2'=Vec -> Vec
                |Mult when e1'=Float&&e2'=Int -> Vec
                |Add|Sub|Mult|Div when e1'=Float && e2'=Float -> Float
                |Add|Sub|Mult|Div when e1'=Int && e2'=Float -> Float 
                |Add|Sub|Mult|Div when e1'=Float && e2'=Int -> Float
                |Equal|Neq when e1'=e2'-> Int 
                |Equal|Neq when e1'=Float && e2'=Int -> Int 
                 |Equal|Neq when e1'=Int && e2'=Float -> Int 
                |Less|Leq|Greater|Geq when e1'=e2' -> Int
                |Less|Leq|Greater|Geq when e1'=Int && e2'=Float -> Int
                |Less|Leq|Greater|Geq when e1'=Float && e2'=Int-> Int
                |And|Or when e1'=Int && e2'=Int -> Int
                | _-> raise(Failure ("Unsupported operands"^ Ast.string_of_expr e1 ^ " "^Ast.string_of_expr e2^" for "
                                     ^(string_of_op op)))
            )
            |Unop(op,e1) -> let e1' = expr_b e1 in
                (match op with
                    Neg when e1'=Int -> Int
                    |Neg when e1'=Float -> Float
                    |Neg when e1'=Vec -> Vec
                    |Pos when e1'=Int -> Int
                    |Pos when e1'=Float -> Float 
                    |Pos when e1'=Vec -> Vec
                    |Preinc when e1'= Int -> Int 
                    |Preinc when e1'= Float -> Float
                    |Predec when e1'= Int -> Int
                    |Predec when e1'= Float -> Float 
                    | _ -> raise(Failure("No unary operator defined for "^ Ast.string_of_expr e1 ))
                )
            |Noexpr -> Void
            |Asnop(e1,asnp,e2)  -> let e1' = expr_b e1 and  e2'=expr_b e2 in 
                (match asnp with
                     Asn when e1'=e2' -> e1'
                    |Asn when e2'=Void -> e1' (*Extermely poor idea but need to figure out constructor problem that return void*)
                    |Asn when e1'=Float && e2'=Int ->  Float
                    |CmpAsn b when e1'=(expr_b (Binop(e1, b, e2))) ->  e1'
                    (*|CmpAsn b when e1'=Float && e2'=Int ->  Float
                    |CmpAsn b when e1'=Int && e2'=Float ->  Float*)
                    | _ -> raise (Failure ("Invalid assigment of " ^ Ast.string_of_typ e2' ^ " to "^Ast.string_of_typ e1'))
                )
            |Call(e1, actuals) -> let e1' = (match e1 with 
                Id s -> (try lookup_function s 
                         with Not_found -> function_decl s)
                |Member (e,s) -> let e'= expr_b e in let
                                     sname= (match e' with
                                            UserType(s,e1) -> s
                                            | _-> raise(Failure("Dot operator on a non-user type"))
                                            )
                                     in get_mem_func_name sname s 
        
                |_-> raise(Failure("here"))
                )
                in
                let fd = e1' in
                    if List.length actuals != List.length fd.params then
                            raise (Failure ("Incorrect number of arguments "))
                    else
                        (* MAY NEED TO REPLACE CHECK_ASS WITH ARG_ASS *)
                         List.iter2 (fun (ft, _,_) e -> let et = expr_b e in
                            ignore (check_ass ft et
                            (Failure ("Illegal actual argument found " ^ Ast.string_of_typ ft ^ " "^Ast.string_of_typ et))))
                    fd.params actuals;
                fd.rettyp
            |Vecexpr (e1,e2) -> Vec
            |Posop (_,e2)-> let e2'=expr_b e2
            in (match e2' with
                |Int -> Int
                |_ -> raise(Failure("Cannot apply PostInc or PostDec " ^ " to " ^ Ast.string_of_expr e2))
            )
            |Trop(_,e1,e2,e3) -> let e1'=expr_b e1 and e2'= expr_b e2 and e3'=expr_b e3
                                 in if(e1'!=Int)
                                    then raise(Failure("Need a Condtional statement"))
                                else(
                                    if(e1'=e2')
                                        then e1'
                                    else if(e1'=Float&&e2'=Int)
                                        then e1'
                                    else if(e1'=Int&&e2'=Float)
                                            then e2'
                                    else 
                                        raise(Failure("Cannot return incompatiable types"))
                                    )
            |Index(e1,e2) -> let e1' = expr_b e1 and e2' = expr_b e2 (* ALLOW VECTOR INDEXING *)
                            in let te1' = (match e1' with
                             Array(t,_) -> t
                             |Vec -> Float
                            | _-> raise(Failure("Indexing a non-array/vector"))
                                )
                            in 
                            if e2'!= Int
                                then raise(Failure ("Must index with an integer "))
                                 else te1'  
            |Member(e1,s) -> let e1' = expr_b e1
                in let te1'= (match e1' with
                        UserType(s1,_) -> s1
                        |_ -> raise(Failure("Dot operator on a non-user type"))
                        )
                    in
                    ( try member_var_type te1' s with Not_found -> raise(Failure(s^" is not a member of "^(string_of_typ e1') )))           
        in 
        let check_bool_expr e = if expr_b e != Int (*Could take in any number need to force check 1 or 0*)
            then raise(Failure((string_of_expr e)^" is not a boolean value."))
            else() in 
        let rec stmt = function
             Block (vl,sl)  -> check_block (vl, sl) scopes
            |Expr e -> ignore(expr_b e)
            |Return e -> let e1' = expr_b e in if e1'= func.rettyp then () else
                raise(Failure("Incorrect return type in " ^ func.fname))
            |If(p,e1,e2) -> check_bool_expr p; stmt e1; stmt e2;
            |For(e1,e2,e3,state) -> ignore(expr_b e1); check_bool_expr e2; ignore(expr_b e3); stmt state
            |While(p,s) -> check_bool_expr p; stmt s 
            |ForDec (vdecls,e2,e3,body) -> stmt  ( Block(vdecls, [For(Noexpr , e2, e3, body)]) )
            |Timeloop(s1,e1,s2,e2,st1) -> ()
            |Frameloop (s1,e1,s2,e2,st1)-> ()
            | Break | Continue -> () (* COMPLICATED: CHECK If in Loop *)
        in 
        let check_ret () = match stmt_list with
            [Return _ ] -> ()
            |Return _ :: _ -> raise(Failure("Can't put more code after return"))
            |_ -> ()
        in
        check_ret(); List.iter stmt stmt_list (* End of check_block *)
    in 
    (* Construct the scopes list before calling check_block *)
    let scopes_list = match func.typ with
                (* Functions can't access members so no struct scope *)
                Func -> [(local_vars, LocalScope); (global_vars, GlobalScope) ]
                (* Don't need  struct scope map as we have one and the type doesn't match as well.
                   So we are using an empty map *)
              | _      -> [(local_vars, LocalScope); (StringMap.empty, StructScope) ; (global_vars, GlobalScope)]
    in 
    check_block (func.locals, func.body) scopes_list
in
List.iter function_check functions;
List.iter (fun sdecl -> List.iter function_check (sdecl.ctor::sdecl.methods)) structs