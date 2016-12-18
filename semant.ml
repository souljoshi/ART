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


    let check_add_shape actuals =
        List.iter(fun (t,_,_) -> (match t with
                    UserType(_,ShapeType) -> ()
                    | _ -> raise(Failure("Must be a shape type"))
                  )) actuals

let struct_build prog =
    let globals = prog.v
    and functions = prog.f

    in  
    (* A map of all struct/shape types *)
    let structs = List.fold_left ( fun m st -> report_dup(fun n-> "Duplicate member variable named " ^n ^" in struct " ^ st.sname)(List.map (fun (t,n) ->  n)st.decls); StringMap.add st.sname st m)
                StringMap.empty prog.s in

                 List.iter(fun fd -> List.iter(fun (t,n)-> (match t with 
                                                UserType(s,_) -> if(fd.sname = s) then raise(Failure("Cannot nest struct/shape "^fd.sname^" within itself ")) else let x= StringMap.mem s structs in if x=false then raise(Failure("Must define struct "^s ^" before using it in struct "^fd.sname))            
                                                | _-> ()   
                                                ))fd.decls) prog.s;
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
            {ss = s.ss;sname = s.sname; decls = s.decls; ctor = if (s.ctor.fname="") then default_ctr s.sname else s.ctor; 
                methods = 
                if s.ss = ShapeType
                    then 
                        let draw = try List.find (fun f2 -> f2.fname = "draw") s.methods
                        with Not_found -> raise (Failure ("No draw method in Shape")) s.methods
                        in 
                            if (draw.rettyp!=Void||draw.params!=[])
                            then raise(Failure("Draw method must have return type Void and no parameters"))
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
(StringMap.add "drawpoint"{rettyp=Void;fname="drawpoint";params=[(Vec,"x",Value)];locals=[];body=[];typ=Func;owner="None";}
(StringMap.singleton "printc" {rettyp=Void; fname="printf";params=[(Char, "x",Value)];locals=[];body=[];typ=Func;owner="None";}))))))))
(*let function_decls =
    List.map(fun fd -> fd.fname) functions*)
in 
List.iter(fun fd -> let x= StringMap.mem fd.fname built_in_fun in if x=true then raise(Failure("Cannot redefine built in function " ^fd.fname)) else ())functions;        
let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                        built_in_fun functions


in
let function_decl s = try StringMap.find s function_decls       (*Builds a string map of name of func to function recored*)
    with Not_found -> raise (Failure ("Unrecognized function " ^ s ^" did you forget to define it ?"))
in

let  main_check = let mn = function_decl "main" 
        in if(mn.rettyp!=Int)
            then raise(Failure("Main must be defined as return type int"))
           else ()(*Makes sure that main is defined*)        
in
    main_check;
let global_vars = List.fold_left(fun m(t,n,_)->StringMap.add n t m) StringMap.empty(globals)
in
let function_check func =

    (* Map of struct name to struct *)
    let struct_name_list = List.fold_left(fun m usr -> StringMap.add usr.sname usr m) (*creates a struct names list*)
        StringMap.empty(structs)
    in 

(* Given struct give map of method names to method *)
    let get_member_funcs name = let st = try StringMap.find name struct_name_list (*creates a struct member funciontlist*)
        with  Not_found -> raise(Failure("Could not find struct "^name))
        in List.fold_left(fun m s -> StringMap.add s.fname s m)
            StringMap.empty st.methods
    in 
    (* Get constructor *)
    let get_member_constr name = let st = StringMap.find name struct_name_list (*creates a struct member funcion list*)
        in st.ctor
    in
    
    (* Get map of memb_variables to their type *)
    let get_struct_member_var name = let st = try StringMap.find name struct_name_list
        with  Not_found -> raise(Failure("Could not find struct "^name))
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

    let formal_vars = List.fold_left(fun m(t,n,_)->StringMap.add n t m) StringMap.empty (func.params)
    in
    (* Top level local_vars *)
    let local_vars =  List.fold_left(fun m(t,n,i)-> (match t with
                                                    UserType(s,t1) -> get_struct_member_var s; StringMap.add n t m
                                                    | _ -> StringMap.add n t m
                                                    ))formal_vars (func.locals)
    in
        report_dup(fun n-> "Duplicate Parameter Named " ^n ^" in " ^ func.fname)(List.map (fun (_,a,_) ->  a)func.params);    (*Checks is there exists duplicate parameter names and function local name*)
        report_dup(fun n-> "Duplicate local Named " ^n ^ " in " ^ func.fname)
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
            report_dup(fun n-> "Duplicate local Name " ^n ^ " in " ^ func.fname)(List.map (fun (_,a,_) ->  a)local_decls);
            let add_local m (t,n) =  StringMap.add n t m in
                 (* NOTE this translation should be moved to the semantic part of the code *)
            let (stmt_list, local_decls) = List.fold_left
                  (* Handle expression initers by adding them as assignment expression statments *)
                  (fun (sl, ld) (t,n,i) -> 
                        ( match i with Exprinit e ->  Expr( Asnop((Id(n), t),Asn, e),Void)::sl , (t,n)::ld  
                          | _  ->  sl, (t,n)::ld (* Silently ingore NoInit and ListInit. HANDLE OTHER INTIIALIZERS*) 
                        )
                  ) (stmt_list, [])
                (* Need to reverse since we are pushing to top of stmt_list. Luckily, the fold unreversed local_decls *)
                (List.rev local_decls)
            in
            let locals =  List.fold_left add_local StringMap.empty local_decls
            in  stmt_list,(locals, LocalScope)::scopes
        in

        let print_scope  scope = 
    List.iter(fun s -> let smap = fst s
    in StringMap.iter( fun s n-> print_endline s) smap
    ) (List.rev scope)

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
                    else raise(Failure("Undeclared variable " ^n ^" in "^func.fname ^ " with owner "^func.owner))
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
            |(Promote s, _) -> (Promote s ,Float)
            |(Binop(e1,op,e2),_) -> let (e1',t1') = (expr_b e1) and (e2',t2')=(expr_b e2) in
            (match op with
                Add|Sub|Mult|Div|Mod when t1'=Int && t2'=Int -> (Binop((e1',t1'),op,(e2',t2')),Int)
                |Add|Sub|Mult|Div when t1'=Vec&&t2'=Vec -> (Binop((e1',t1'),op,(e2',t2')),Vec)
                |Mult when t1'=Vec&&t2'=Int ->  (Binop((e1',t1'),op,(Promote((e2',t2')),Float)),Vec)
                |Mult when t1'=Vec&&t2'=Float -> (Binop((e1',t1'),op,(e2',t2')),Vec)
                |Mult when t1'=Int&&t2'=Vec ->  (Binop((Promote((e1',t1')),Float),op,(e2',t2')),Vec)
                |Mult when t1'=Float&&t2'=Vec -> (Binop((e1',t1'),op,(e2',t2')),Vec)
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
                | _-> raise(Failure ("Unsupported operands"^ Ast.string_of_expr e1 ^ " "^Ast.string_of_expr e2^" for "
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
                                            |_ -> raise(Failure("Cannot apply PreInc or PreDec " ^ " to " ^ Ast.string_of_expr f))
                                            )
                    |Predec when t1'= Int -> (match f with
                                            (Id(s1),Int) -> (Unop(op,(f)),Int)
                                            |(Index(e1,e2),Int) -> (Unop(op,(f)),Int)
                                            |_ -> raise(Failure("Cannot apply PreInc or PreDec " ^ " to " ^ Ast.string_of_expr f))
                                            )
                    | _ -> raise(Failure("No unary operator defined for "^ Ast.string_of_expr e1 ))
                )
            |(Noexpr,_) -> (Noexpr,Void)
            |(Asnop(e1,asnp,e2),_)  -> let (e1',t1') = (expr_b e1) and  (e2',t2')=(expr_b e2) in 
                (match asnp with
                     Asn when t1'=t2' -> (Asnop((e1',t1'),asnp,(e2',t2')),t1')
                    |Asn when t2'=Void -> (Asnop((e1',t1'),asnp,(e2',t2')),t1') (*Extermely poor idea but need to figure out constructor problem that return void*)
                    |Asn when t1'=Float && t2'=Int ->  (Asnop((e1',t1'),asnp,(e2',t2')),Float)
                    |CmpAsn b when t1'=snd(expr_b (Binop((e1',t1'), b, (e2',t2')), Void)) ->  (Asnop ((e1',t1'),asnp,(e2',t2')),t1')
                    (*|CmpAsn b when e1'=Float && e2'=Int ->  Float
                    |CmpAsn b when e1'=Int && e2'=Float ->  Float*)
                    | _ -> raise (Failure ("Invalid assigment of " ^ Ast.string_of_typ t2' ^ " to "^Ast.string_of_typ t1'))
                )
            |(Call(e1, actuals),_) -> let e1' = (match e1 with 
                ((Id s),_) -> (try lookup_function s 
                         with Not_found -> function_decl s)
                |(Member (e,s),_)-> let e'= snd(expr_b e) in let
                                     sname= (match e' with
                                            UserType(s,e1) -> s
                                            | _-> raise(Failure("Dot operator on a non-user type"))
                                            )
                                     in get_mem_func_name sname s 
        
                |_-> raise(Failure("here"))
                )
                in 
                let fd = e1' in
                    if (List.length actuals != List.length fd.params) || (fd.fname="addshape") 
                    then
                        if fd.fname="addshape"&&(List.length actuals>0) 
                        then 
                            List.iter(fun e -> let (e1',t1')=(expr_b e) in 
                                (match t1' with
                                UserType(_,ShapeType) -> ()
                                | _-> raise(Failure("Can only add shapes to addshape")) 
                                )
                            ) actuals
                        else raise (Failure ("Incorrect number of arguments in "^fd.fname))
                    else 
                        if fd.fname <>"addshape" 
                        then
                        (
                            List.iter2 (fun (t, s, p) e -> if p = Ref then 
                            let e = (expr_b e) in
                                match e with 
                                    (Id(s),_) -> ()
                                    | (Member(e,s),_) -> ()
                                    | (Index(e,_),_) -> 
                                        (match e with  
                                        | (Id _, Vec) | (_,Array(_,_)) -> ()
                                        | (_,t) as e-> raise(Failure("Illegal pass by reference."))
                                        ) 
                                    | _ -> raise(Failure("Illegal pass by reference."))  
                            ) fd.params actuals;

                            List.iter2 (fun (ft, _,_) e -> let et = snd(expr_b e) in
                                ignore (check_ass ft et
                                (Failure ("Illegal actual argument found " ^ Ast.string_of_typ ft ^ " "^Ast.string_of_typ et^ " in function "^fd.fname)))
                            ) fd.params actuals;
                        )
                        else ();        
                (Call(e1,actuals),fd.rettyp)
            |(Vecexpr (e1,e2),_) -> 
                let (e1',t1') = (expr_b e1) and (e2',t2') = (expr_b e2)
                in
                if (t1' != Float || t2' != Float)
                    then raise(Failure("Elements of Vector must be floats."))
                else (Vecexpr((e1',t1'),(e2',t2')),Vec)
            |(Posop (s,e2),_)-> let e2'=(expr_b e2)
            in (match e2' with
                (Id(s1),Int) -> (Posop(s,(e2')),Int)
                |(Index(e1,e2),Int) -> (Posop(s,(e2')),Int)
                |_ -> raise(Failure("Cannot apply PostInc or PostDec " ^ " to " ^ Ast.string_of_expr e2))
            )

            |(Trop(sz,e1,e2,e3),_) -> let (e1',t1')=(expr_b e1) and (e2',t2')=(expr_b e2) and (e3',t3')=(expr_b e3)
                                 in if(t1'!=Int)
                                    then raise(Failure("Need a Condtional statement"))
                                else(
                                    if(t1'=t2')
                                        then (Trop(sz,(e1',t1'),(e2',t2'),(e3',t3')),t1')
                                    else if(t1'=Float&&t2'=Int)
                                        then (Trop(sz,(e1',t1'),(Promote((e2',t2')),Float),(e3',t3')),t1')
                                    else if(t1'=Int&&t2'=Float)
                                            then (Trop(sz,(Promote((e1',t1')),Float),(e2',t2'),(e3',t3')),t1')
                                    else 
                                        raise(Failure("Cannot return incompatiable types"))
                                    )
            |(Index(e1,e2),_) -> let (e1',t1') = (expr_b e1) and (e2',t2') = (expr_b e2) (* ALLOW VECTOR INDEXING *)
                            in let te1' = (match t1' with
                             Array(t,_) -> t
                             |Vec -> Float
                            | _-> raise(Failure("Indexing a non-array/vector"))
                                )
                            in 
                            if t2'!= Int
                                then raise(Failure ("Must index with an integer "))
                                 else (Index((e1',t1'),(e2',t2')),te1')  

            |(Member(e1,s),_) -> let (e1',t1') = (expr_b e1)
                in let te1'= (match t1' with
                        UserType(s1,_) -> s1
                        |_ -> raise(Failure("Dot operator on a non-user type"))
                        )
                    in
                    ( try (Member((e1',t1'),s),member_var_type te1' s) with Not_found -> raise(Failure(s^" is not a member of "^(string_of_typ t1'))))  
                
        | _ -> (Noexpr,Void)        
        in 
    
    let check_bool_expr e = if snd(expr_b e) != Int (*Could take in any number need to force check 1 or 0*)
            then raise(Failure((string_of_expr e)^" is not a boolean value."))
            else() in
    List.iter(fun (t,n,e) -> ( match e with 
                                    Exprinit e -> let (e1',t1') = (expr_b e)
                                                        in if(t1'=t)
                                                            then ()
                                                        else raise(Failure("Global var "^n^" needs to be assigned to type "^string_of_typ t^ " not "^string_of_typ t1'))
                                    |_ -> ()
                                    )) globals;

    List.iter(fun (t,n,e1) -> ( match t with 
                                  Array(_,e)  -> let (e1',t1') = (expr_b e) 
                                                        in
                                                        (match e1' with
                                                        | IntLit _ -> ()
                                                        |_->  raise(Failure("Cannot declare any array without a intger constant not variables ")))           
                                    |_ -> ()
                                    )) func.locals;


        let rec stmt = function
             Block (vl,sl,_)  -> check_block (vl, sl) scopes
            |Expr e -> ignore(expr_b e)
            |Return e -> let e1' = snd(expr_b e) in if e1'= func.rettyp then () else
                raise(Failure("Incorrect return type in " ^ func.fname))
            |If(p,e1,e2) -> check_bool_expr p; stmt e1; stmt e2;
            |For(e1,e2,e3,state) -> ignore(expr_b e1); check_bool_expr e2; ignore(expr_b e3); stmt state
            |While(p,s) -> check_bool_expr p; stmt s 
            |ForDec (vdecls,e2,e3,body) -> stmt  ( Block(vdecls, [For((Noexpr,Void) , e2, e3, body)],PointContext) )
            |Timeloop(s1,e1,s2,e2,st1) -> 
                (* Need to check statements also ? *)
                if s1 = s2 then raise(Failure("Duplicate variable name in timeloop definition."))
                else
                    let e1' = snd(expr_b e1) 
                    and e2' = snd(expr_b e2)
                    in 
                    if e1' = Float && e2' = Float 
                        then ()
                    else raise(Failure("Only float expressions are accepted in timeloop definition."))
            |Frameloop (s1,e1,s2,e2,st1)-> 
                (* Need to check statements also ? *)
                if s1 = s2 then raise(Failure("Duplicate variable name in frameloop definition."))
                else
                    let e1' = snd(expr_b e1) 
                    and e2' = snd(expr_b e2)
                    in 
                    if e1' = Float && e2' = Float 
                        then ()
                    else raise(Failure("Only float expressions are accepted in frameloop definition."))
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
