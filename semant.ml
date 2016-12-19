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
    if lval = rval then lval else raise err                        (*Since this is a special case I supsend the one to one checking and just use rval*)

let struct_build prog =
    let globals = prog.v
    and functions = prog.f

    in  
    (* A map of all struct/shape types *)
    let structs = List.fold_left ( 
        fun m st -> report_dup(fun n-> "Duplicate member variable named " ^n ^" in "^(
          string_of_stosh st.ss)^" "^st.sname)(List.map (fun (t,n) ->  n)st.decls);

          (List.iter(fun (t,n)-> 
            (match t with 
                UserType(s,ss) -> if(st.sname = s) 
                    then raise(Failure("Cannot nest "^(string_of_stosh st.ss)^" "^st.sname^" within itself")) 
                    else (
                        try if (StringMap.find s m).ss != ss then  raise Not_found with Not_found ->
                        raise(Failure((string_of_stosh ss)^" "^s ^" must be defined before using in "^(string_of_stosh st.ss)^" "^st.sname)) 
                    )           
                | _-> ()   
            ))st.decls);
          StringMap.add st.sname st m

    )

                StringMap.empty prog.s in


    let (structs,funcs) = (* Refers to structs and non-member functions *)
        (* Puts methods and constructors with appropriate struct and returns tuple
            (map, bool) *)
        let filter_func m f =
          match f.typ with
            Func -> (m, true) (* true means keep function *)
          | Constructor -> let s = try StringMap.find f.owner m
                    with Not_found -> raise (Failure ("Constructor of undefined struct/shape: " ^ f.owner^"::"^f.fname))
                in if (s.ctor.fname="") then (StringMap.add s.sname
                            {ss = s.ss;sname = s.sname; decls = s.decls; ctor = f; methods = s.methods} m , false)
                        else
                            raise(Failure("Multiple constructor definitions for "^(string_of_stosh s.ss)^ " "^s.sname))
          | Method -> let s = try StringMap.find f.owner m
                    with Not_found -> raise (Failure ("Method of undefined struct/shape: " ^ f.owner^"::"^f.fname))
                in try ignore( List.find (fun f2 -> f2.fname = f.fname) s.methods);
                       raise(Failure("Multiple definitions for method " ^f.owner^"::"^f.fname))  
                    with Not_found -> (StringMap.add s.sname
                                {ss = s.ss;sname = s.sname; decls = s.decls; ctor = s.ctor; methods = f::s.methods} m , false)
        in
        List.fold_left ( fun (m,l) f -> let (m, cond) = filter_func m f in
        if cond then (m, f::l) else (m, l) ) (structs, []) functions
    in
    { s = List.map (fun st -> let s = StringMap.find st.sname structs in
            (* If no contructor is defined add default *)
            {ss = s.ss;sname = s.sname; decls = s.decls; ctor = if (s.ctor.fname="") then default_ctr s.sname else s.ctor; 
                methods = 
                if s.ss = ShapeType
                    then 
                        let draw = try List.find (fun f2 -> f2.fname = "draw") s.methods
                        with Not_found -> raise (Failure ("draw method not defined in shape "^s.sname))
                        in 
                            if (draw.rettyp!=Void||draw.params!=[])
                            then raise(Failure("draw method must have return type void, and no parameters"))
                            else s.methods       
                else s.methods}
        ) prog.s;
      f = List.rev funcs ; v = globals }

let check prog =
    (* Get the global variables and functions *)
    let globals = prog.v
    and functions = prog.f
    and structs = prog.s
    in
    
        report_dup(fun n-> "Duplicate function name " ^n)(List.map (fun fd -> fd.fname)functions); (*Does pretty basic superfical checking if there exists duplicate function names, global or structs*)

        report_dup(fun n-> "Duplicate global variable " ^n)(List.map (fun (_,a,_) ->  a)globals);

        report_dup(fun n-> "Duplicate struct/shape name " ^n)(List.map(fun st -> st.sname)structs); 

let built_in_fun = StringMap.add "printi"
{rettyp=Void; fname="printi";params=[(Int, "x",Value)];locals=[];body=[];typ=Func;owner="None"}
(StringMap.add"printf" {rettyp=Void; fname="printf";params=[(Float, "x",Value)];locals=[];body=[];typ=Func;owner="None";}       (*Builds a list of the predefined functions special note about add shape it demands a name to contstruct the type *)
(StringMap.add "prints" {rettyp=Void; fname="prints";params=[(String, "x",Value)];locals=[];body=[];typ=Func;owner="None";}
(StringMap.add "addshape"{rettyp=Void;fname="addshape";params=[(UserType(".shape",ShapeType),"x",Value)];locals=[];body=[];typ=Func;owner="None";}
(StringMap.add "cos"{rettyp=Float;fname="cos";params=[(Float,"x",Value)];locals=[];body=[];typ=Func;owner="None";}
(StringMap.add "sin"{rettyp=Float;fname="sin";params=[(Float,"x",Value)];locals=[];body=[];typ=Func;owner="None";}
(StringMap.add "setcolor"{rettyp=Void;fname="setcolor";params=[(Float,"x",Value);(Float,"x",Value);(Float,"x",Value)];locals=[];body=[];typ=Func;owner="None";}
(StringMap.add "drawpoint"{rettyp=Void;fname="drawpoint";params=[(Vec,"x",Value)];locals=[];body=[];typ=Func;owner="None";}
(StringMap.singleton "printc" {rettyp=Void; fname="printc";params=[(Char, "x",Value)];locals=[];body=[];typ=Func;owner="None";}))))))))
(*let function_decls =
    List.map(fun fd -> fd.fname) functions*)
in 
List.iter(fun fd -> let x= StringMap.mem fd.fname built_in_fun in if x=true then raise(Failure("Built-in function " ^fd.fname^ " cannot be redefined")) else ())functions;        
let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                        built_in_fun functions


in
let function_decl s = try StringMap.find s function_decls       (*Builds a string map of name of func to function recored*)
    with Not_found -> raise (Failure ("Unrecognized function " ^ s ^". Did you forget to define it?"))
in

let  main_check = let mn = (try function_decl "main" with Failure _ -> raise( Failure "Must define main"))
        in if(mn.rettyp!=Int)
            then raise(Failure("main function must have return type int"))
           else ()(*Makes sure that main is defined*)        
in
    main_check;
(* Map of struct name to struct *)
let struct_name_list = List.fold_left(fun m usr -> StringMap.add usr.sname usr m) (*creates a struct names list*)
    StringMap.empty(structs)
in 

(* returns type and improved initializer *)
let rec check_global_initer t n = function
    Exprinit e -> let (_,t') as e' = const_expr e in
             let e'' = (try do_lit_promote e' t' t with Failure _ ->raise(Failure("Invalid initializer for global var "^n)))
             in (t, Exprinit(e''))
  | Listinit il -> (match t with
        Array(t2,e) -> 
          let l =  List.map ( fun i -> snd(check_global_initer t2 n i)) il in 
          if e <> (Noexpr,Void) then (t, Listinit l) else ( Array(t2, (IntLit (List.length l), Int)), Listinit l)
      | UserType(s,_) ->
            (* type of members *)
            let dtl = List.map (fun (t,_)-> t) (StringMap.find s struct_name_list).decls  in
            if (List.length dtl = List.length il) then
                (t, Listinit(List.map2 (fun t i -> snd(check_global_initer t n i)) dtl il))
            else raise(Failure("Invalid initializer for global var "^n))

      | _ -> raise(Failure("Nested initializer cannot be used with "^(string_of_typ t)))
    ) 
  | Noinit -> (t,Noinit)

in

(* Simultaneously modify globals list and build global_vars map *)
let (global_vars, globals') = 
        List.fold_left(fun (m,gl) (t,n,i)->
            (* check if type exists *)
            (match t with
                UserType(s,ss) -> ignore(
                    try if (StringMap.find s struct_name_list).ss != ss then  raise Not_found with Not_found ->
                        raise(Failure((string_of_stosh ss)^" "^s ^" is not defined "))
                    );
                | Array(_,(Noexpr,Void)) when i=Noinit -> raise(Failure("Incomplete array without initializer: "^n))
                | Array(_,(Noexpr,Void)) when i<>Noinit -> ()
                | Array(_,e) -> (try if snd(const_expr e)<>Int then raise Not_found with Not_found|Failure _ -> 
                                        raise(Failure("Array declaration requires constant integer: "^n)))
                | _ -> ()
            ); 
            let (t',i') = check_global_initer t n i in
            (StringMap.add n t' m, (t',n,i')::gl)
        ) (StringMap.empty, []) globals
in
let globals = List.rev globals'
in

let function_check func =


(* Given struct give map of method names to method *)
    let get_member_funcs name = let st = try StringMap.find name struct_name_list (*creates a struct member funciontlist*)
        with  Not_found -> raise(Failure("Undefined struct/shape "^name))
        in List.fold_left(fun m s -> StringMap.add s.fname s m)
            StringMap.empty st.methods
    in 
    (* Get constructor *)
    let get_member_constr name = let st = StringMap.find name struct_name_list (*creates a struct member funcion list*)
        in st.ctor
    in
    
    (* Get map of memb_variables to their type *)
    let get_struct_member_var name = let st = try StringMap.find name struct_name_list
        with  Not_found -> raise(Failure("Undefined struct/shape "^name))
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
            with Not_found -> raise(Failure(func ^ " is not a method of " ^name))
    in

    let formal_vars = List.fold_left(fun m(t,n,_)->StringMap.add n t m) StringMap.empty (func.params)
    in
    (* Top level local_vars *)
    let local_vars =  List.fold_left(fun m(t,n,_)-> 
          (match t with
              UserType(s,ss) -> ignore(
                            try if (StringMap.find s struct_name_list).ss != ss then  raise Not_found with Not_found ->
                                raise(Failure((string_of_stosh ss)^" "^s ^" is not defined "))
                            ); StringMap.add n t m
              | _ -> StringMap.add n t m
          )
        )formal_vars (func.locals)
    in
        report_dup(fun n-> "Duplicate parameter " ^n ^" in " ^ func.fname)(List.map (fun (_,a,_) ->  a)func.params);    (*Checks is there exists duplicate parameter names and function local name*)
        report_dup(fun n-> "Duplicate local variable " ^n ^ " in " ^ func.fname)
            ((List.map (fun (_,a,_) ->  a)func.params)@(List.map (fun (_,a,_) ->  a)func.locals));  
    
    
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
            report_dup(fun n-> "Duplicate local variable " ^n ^ " in " ^ func.fname)(List.map (fun (_,a,_) ->  a)local_decls);
            let add_local m (t,n,i) = 
                    ignore(match t with
                        UserType(s,ss) -> ignore(
                            try if (StringMap.find s struct_name_list).ss != ss then  raise Not_found with Not_found ->
                                raise(Failure((string_of_stosh ss)^" "^s ^" is not defined "))
                            ) 
                        | Array(_,(Noexpr,Void)) when i=Noinit -> raise(Failure("Incomplete array without initializer: "^n))
                        | Array(_,(Noexpr,Void)) when i<>Noinit -> ()
                        | Array(_,e) -> (try if snd(const_expr e)<>Int then raise Not_found with Not_found|Failure _ -> 
                                        raise(Failure("Array declaration requires constant integer: "^n)))
                        | _ -> ()
                    ); StringMap.add n t m
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
                    try member_var_type func.owner n  with Not_found-> _ret_type n (List.tl scopes) )
                )
                with Not_found -> 
                    if func.owner = ""
                    then raise(Failure("Undeclared variable " ^n ^" in "^func.fname))
                    else raise(Failure("Undeclared variable " ^n ^" in "^ func.owner^ "::"^func.fname ))
        in
        (* Gets type for variable name s [old ret_type]*)
        let ret_type n = _ret_type n scopes 
        in
        (* Gets type of expression. [LATER ON HANDLE WITH SAST] *)
        let rec expr_b  = function
             (IntLit s,_)-> (IntLit s,Int)
            |(CharLit s, _)-> (CharLit s, Char)
            |(StringLit s,_)-> (StringLit s, String)
            |(FloatLit s,_)-> (FloatLit s, Float)
            |(VecLit (f1,f2),_)-> (VecLit(f1,f2), Vec)
            |(Id s, _) as s1 ->  (Id s, ret_type s)
            |(Promote _, _) as p-> p (* Promote can not be generated by parser *)
            |(Binop(e1,op,e2),_) -> let (e1',t1') = (expr_b e1) and (e2',t2')=(expr_b e2) in

            (match op with
                Add|Sub|Mult|Div|Mod when t1'=Int && t2'=Int -> (Binop((e1',t1'),op,(e2',t2')),Int)
                |Add|Sub|Mult|Div when t1'=Vec&&t2'=Vec -> (Binop((e1',t1'),op,(e2',t2')),Vec)
                |Mult|Div when t1'=Vec&&t2'=Int ->  (Binop((e1',t1'),op,(Promote((e2',t2')),Float)),Vec)
                |Mult|Div when t1'=Vec&&t2'=Float -> (Binop((e1',t1'),op,(Promote((e2',t2')),Vec)),Vec)
                |Mult when t1'=Int&&t2'=Vec ->  (Binop((Promote((e1',t1')),Float),op,(e2',t2')),Vec)
                |Mult when t1'=Float&&t2'=Vec -> (Binop((Promote((e1',t1')),Vec),op,(e2',t2')),Vec)
                |Add|Sub|Mult|Div when t1'=Float && t2'=Float -> (Binop((e1',t1'),op,(e2',t2')),Float)
                |Add|Sub|Mult|Div when t1'=Int && t2'=Float -> (Binop((Promote(e1',t1'),Float),op,(e2',t2')),Float)
                |Add|Sub|Mult|Div when t1'=Float && t2'=Int -> (Binop((e1',t1'),op,(Promote((e2',t2')),Float)),Float)
                |Equal|Neq when t1'=t2'-> (Binop((e1',t1'),op,(e2',t2')),Int)
                |Equal|Neq when t1'=Float && t2'=Int -> (Binop((Promote(e1',t1'),Float),op,(e2',t2')),Int)
                |Equal|Neq when t1'=Int && t2'=Float -> (Binop((e1',t1'),op,(Promote((e2',t2')),Float)),Int)
                |Equal|Neq when t1'=Vec && t2'=Vec -> (Binop((e1',t1'),op,(e2',t2')),Int)
                |Less|Leq|Greater|Geq when t1'=t2' -> (Binop((e1',t1'),op,(e2',t2')),Int)
                |Less|Leq|Greater|Geq when t1'=Int && t2'=Float -> (Binop((Promote(e1',t1'),Float),op,(e2',t2')),Int)
                |Less|Leq|Greater|Geq when t1'=Float && t2'=Int-> (Binop((e1',t1'),op,(Promote((e2',t2')),Float)),Int)
                |And|Or when t1'=Int && t2'=Int -> (Binop((e1',t1'),op,(e2',t2')),Int)
                | _-> raise(Failure ("Unsupported operands "^ Ast.string_of_expr e1 ^ " and "^Ast.string_of_expr e2^" for "
                                     ^(string_of_op op)))
            )
            |(Unop(op,e1),_) -> let (e1',t1') as f = (expr_b e1) in
                (match op with
                     Neg when t1'=Int -> (Unop(op,(e1',t1')),Int)
                    |Neg when t1'=Float -> (Unop(op,(e1',t1')),Float)
                    |Neg when t1'=Vec -> (Unop(op,(e1',t1')),Vec)
                    |Pos when t1'=Int -> (Unop(op,(e1',t1')),Int)
                    |Pos when t1'=Float -> (Unop(op,(e1',t1')),Float) 
                    |Pos when t1'=Vec -> (Unop(op,(e1',t1')),Vec)
                    |Preinc when t1'= Int ->(match f with
                                            (Id(s1),Int) -> (Unop(op,(f)),Int)
                                            |(Index(e1,e2),Int) -> (Unop(op,(f)),Int)
                                            |(Member(_,_),Int) -> (Unop(op,(f)),Int)
                                            |_ -> raise(Failure("PreInc or PreDec cannot be applied to " ^ Ast.string_of_expr f))
                                            )
                    |Predec when t1'= Int -> (match f with
                                            (Id(s1),Int) -> (Unop(op,(f)),Int)
                                            |(Index(e1,e2),Int) -> (Unop(op,(f)),Int)
                                            |(Member(_,_),Int) -> (Unop(op,(f)),Int)
                                            |_ -> raise(Failure("PrecInc or PreDec cannot be applied to " ^ Ast.string_of_expr f))
                                            )
                    | _ -> raise(Failure("Unsupported unary operation for "^ Ast.string_of_expr e1 ))
                )
            |(Noexpr,_) -> (Noexpr,Void)
            |(Asnop(e1,asnp,e2),_)  -> let (e1',t1') = (lexpr_b e1) and  (e2',t2')=(expr_b e2) in 
                (match asnp with
                     Asn when t1'=t2' -> (Asnop((e1',t1'),asnp,(e2',t2')),t1')
                    |Asn when t1'=Float && t2'=Int ->  (Asnop((e1',t1'),asnp,(Promote(e2',t2'),Float)),Float)
                    |CmpAsn b when t1'=snd(expr_b (Binop((e1',t1'), b, (e2',t2')), Void)) ->  (Asnop ((e1',t1'),asnp,(e2',t2')),t1')
                    (*|CmpAsn b when e1'=Float && e2'=Int ->  Float
                    |CmpAsn b when e1'=Int && e2'=Float ->  Float*)
                    | _ -> raise (Failure ("Invalid assigment of " ^ Ast.string_of_typ t2' ^ " to "^Ast.string_of_typ t1'))
                )
            |(Call(e1, actuals),_) -> let (e',fd) = (match e1 with 
                ((Id s),_) as f -> (f, (try lookup_function s 
                                with Not_found -> function_decl s))
                |(Member (e,s),_)-> let (_,t') as f = expr_b e in
                                let sname= (match t' with
                                            UserType(s,e1) -> s
                                            | _-> raise(Failure("Member operator (dot) can only be applied to struct or shape"))
                                            )
                                     in ((Member(f,s), Void), get_mem_func_name sname s)
        
                |_-> raise(Failure("Invalid function call: " ^ string_of_expr e1))
                )
                in 

                let  actuals' = 
                    (
                    if (List.length actuals != List.length fd.params) || (fd.fname="addshape") 
                    then
                        if fd.fname="addshape"&&(List.length actuals>0) 
                        then 
                            List.map(fun e -> let (e1',t1') as f=(expr_b e) in 
                                (match t1' with
                                UserType(_,ShapeType) -> f
                                | _-> raise(Failure("Arguments of addshape function must be of type shape")) 
                                )
                            ) actuals
                        else raise (Failure ("Incorrect number of arguments in "^fd.fname))
                    else 
                        
                        let actuals = List.map2 (fun (t, s, p) e -> if p = Ref then  (lexpr_b e) else (expr_b e)
                        ) fd.params actuals in

                        List.iter2 (fun (ft, _,_) e -> let et = snd(e) in
                            ignore (check_ass ft et
                            (Failure ("Illegal argument "^(string_of_expr e)^" of type "^Ast.string_of_typ et^ " in call to function "^fd.fname^ " which expects argument of type " ^ Ast.string_of_typ ft)))
                        ) fd.params actuals;
                        actuals
                        
                    )   in    
                (Call(e',actuals'), if fd.typ <> Constructor then fd.rettyp
                                else UserType(fd.owner,(StringMap.find fd.owner struct_name_list).ss) )
            |(Vecexpr (e1,e2),_) -> 
                let (e1',t1') as f1 = (expr_b e1) and (e2',t2') as f2= (expr_b e2)
                in
                let f1 = if (t1' = Float) then f1 else if t1'=Int then (Promote f1,Float) else raise(Failure("Elements of vector must be of type double"))
                and f2 = if (t2' = Float) then f2 else if t2'=Int then (Promote f2,Float) else raise(Failure("Elements of vector must be of type double"))
                in (Vecexpr(f1,f2),Vec)
            |(Posop (s,e2),_)-> let e2'=(expr_b e2)
            in (match e2' with
                 (Id(s1),Int) -> (Posop(s,(e2')),Int)
                |(Index(e1,e2),Int) -> (Posop(s,(e2')),Int)
                |(Member(_,_),Int) -> (Posop(s,e2'),Int)
                |_ -> raise(Failure("PostInc or PostDec cannot be applied to " ^ Ast.string_of_expr e2))
                )
            |(Index(e1,e2),_) -> let (e1',t1') = (expr_b e1) and (e2',t2') = (expr_b e2) (* ALLOW VECTOR INDEXING *)
                            in let te1' = (match t1' with
                               Array(t,_) -> t
                             | Vec -> Float
                             | _-> raise(Failure("Indexing only supported for arrays and vectors"))
                                )
                            in 
                            if t2'!= Int
                                then raise(Failure ("Must index with an integer"))
                                 else (Index((e1',t1'),(e2',t2')),te1')  

            |(Member(e1,s),_) -> let (e1',t1') = (expr_b e1)
                in let te1'= (match t1' with
                        UserType(s1,_) -> s1
                        |_ -> raise(Failure("Member operator (dot) can only be applied to struct or shape"))
                        )
                    in
                    ( try (Member((e1',t1'),s),member_var_type te1' s) with Not_found -> raise(Failure(s^" is not a member of "^(string_of_typ t1'))))  
                
        | _ -> (Noexpr,Void)
        (* Special handling for lvalue expressions *)
        and lexpr_b  = function
                (Id s,_) -> (Id s, ret_type s)
              | (Index(e1,e2),_) ->  let (e1',t1') = (lexpr_b e1) and (e2',t2') = (expr_b e2) (* ALLOW VECTOR INDEXING *)
                            in let te1' = (match t1' with
                               Array(t,_) -> t
                             | Vec -> Float
                             | _-> raise(Failure("Indexing only supported for arrays and vectors"))
                                )
                            in 
                            if t2'!= Int
                                then raise(Failure ("Must index with an integer"))
                                 else (Index((e1',t1'),(e2',t2')),te1')  

              | (Member(e1,s),_) -> let (e1',t1') = (lexpr_b e1)
                in let te1'= (match t1' with
                        UserType(s1,_) -> s1
                        |_ -> raise(Failure("Member operator (dot) can only be applied to struct or shape"))
                        )
                    in
                    ( try (Member((e1',t1'),s),member_var_type te1' s) with Not_found -> raise(Failure(s^" is not a member of "^(string_of_typ t1'))))
              | e -> raise (Failure ("rvalue given: "^(string_of_expr e)^ " where lvalue expected"))        
        in 
    
    let check_bool_expr e = let (_,t') as f = expr_b e in
            if t' != Int (*Could take in any number need to force check 1 or 0*)
            then raise(Failure((string_of_expr e)^" is not a boolean value"))
            else f in


        let rec stmt = function
             Block (vl,sl,c)  -> let f =(check_block (vl, sl) scopes) in Block(fst f, snd f, c)
            |Expr e -> Expr(expr_b e)
            |Return e -> let (_,t1') as f= (expr_b e) in if t1'= func.rettyp then Return(f) else
                raise(Failure("Incorrect return type in " ^ func.fname))
            |If(p,e1,e2) -> If(check_bool_expr p, stmt e1, stmt e2)
            |For(e1,e2,e3,state) -> For(expr_b e1, check_bool_expr e2,expr_b e3, stmt state)
            |While(p,s) -> While(check_bool_expr p, stmt s)
            |ForDec (vdecls,e2,e3,body) -> stmt  ( Block(vdecls, [For((Noexpr,Void) , e2, e3, body)],PointContext) )
            |Timeloop(s1,e1,s2,e2,st1) -> 
                (* Need to check statements also ? *)
                if s1 = s2 then raise(Failure("Duplicate variable "^s1^" in timeloop definition"))
                else
                    let (_,t1') as f1 = expr_b e1 
                    and (_,t2') as f2 = expr_b e2
                    in 
                    if (t1' = Float || t1'=Int) && (t2' = Float || t2'=Int) 
                    then Timeloop(s1, (Promote f1,Float), s2, (Promote f2,Float),stmt st1) 
                    else raise(Failure("Timeloop definition only accepts expressions of type double"))
            |Frameloop (s1,e1,s2,e2,st1)-> 
                (* Need to check statements also ? *)
                if s1 = s2 then raise(Failure("Duplicate variable "^s1^" in timeloop definition"))
                else
                    let (_,t1') as f1 = expr_b e1 
                    and (_,t2') as f2 = expr_b e2
                    in 
                    if (t1' = Float || t1'=Int) && (t2' = Float || t2'=Int) 
                    then Frameloop(s1, (Promote f1,Float), s2, (Promote f2,Float),stmt st1) 
                    else raise(Failure("Timeloop definition only accepts expressions of type double"))
        in 
        let check_ret () = match stmt_list with
            [Return _ ] -> ()
            |Return _ :: _ -> raise(Failure("Cannot have any code after a return statement"))
            |_ -> ()
        in

        check_ret(); (local_decls, List.map stmt stmt_list) (* End of check_block *)
    in 
    (* Construct the scopes list before calling check_block *)
    let scopes_list = match func.typ with
                (* Functions can't access members so no struct scope *)
                Func -> [(local_vars, LocalScope); (global_vars, GlobalScope) ]
                (* Don't need  struct scope map as we have one and the type doesn't match as well.
                   So we are using an empty map *)
              | _      -> [(local_vars, LocalScope); (StringMap.empty, StructScope) ; (global_vars, GlobalScope)]
in 
    let (locals',body') = check_block (func.locals, func.body) scopes_list in
    { rettyp = func.rettyp; fname = func.fname; params = func.params; locals = locals'; body = body'; typ = func.typ; owner= func.owner}
in
let f' = List.map function_check functions in
let s' = List.map (fun st-> 
    {
        ss = st.ss ; sname = st.sname ; decls = st.decls;
        ctor = function_check st.ctor;
        methods = List.map function_check st.methods 
    }
) structs in
let v' = globals in
{ s = s' ; f=f'; v =v';}