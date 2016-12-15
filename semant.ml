open Ast

module StringMap = Map.Make(String)
type scope = GlobalScope | LocalScope 
(* Semantic checking of a program. Returns possibly modified Ast if successful,
   throws an exception if something is wrong. *)
let report_dup  exceptf list =
        let rec helper = function 
            n1 :: n2 ::_ when n1=n2 -> raise (Failure(exceptf n1) )
            |_ :: t -> helper t
            |[]->()

        in helper(List.sort compare list)
let check_ass lval rval err =
    if lval = rval then lval else 
    (match lval with 
        UserType(s,s1) when s="DUMMY" ->rval (*Since DrawPoint demands a string that it will use as a type I give it a String Dummy and will past semant*)
        |_-> raise err
    )

let rec string_of_expr (*e = "( "^ paren_of_expr e ^ " )"
and 
paren_of_expr *) = function
    IntLit(l) -> string_of_int l
  | CharLit(l) -> "'" ^ (string_of_chr l) ^ "'"
  | FloatLit(l) -> string_of_float l
  | StringLit(s) -> "" ^ s
  | VecLit(a,b)  -> "< " ^ (string_of_float a) ^ " , " ^ (string_of_float b) ^ " >"
  | Id(s) -> s
  | Vecexpr(e1,e2) -> " < "^ string_of_expr e1 ^ " , " ^ string_of_expr e2 ^ " >"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Asnop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_asnop o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Posop(o, e) -> string_of_expr e ^ string_of_pop o
  | Trop (o, e1, e2, e3) -> let t = strings_of_trop o in
            string_of_expr e1 ^ fst t ^ string_of_expr e2 ^ snd t ^ string_of_expr e3
  | Call(f, el) ->
      string_of_expr f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Index(e1, e2) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
  | Member(e1, s) -> string_of_expr e1 ^ "." ^ s
  | Noexpr -> ""


let rec string_of_typ = function
    Int -> "int"
  | Char -> "char"
  | Void -> "void"
  | Float -> "double"
  | Vec  -> "vec"
  | String -> "string"
  | UserType(n,ss) -> string_of_stosh ss ^ n
  | Array(_, _) as a -> let (t,l) = list_of_arr a
     in string_of_typ t ^ String.concat "" (List.map (fun e -> "[" ^ string_of_expr e ^ "]") l)

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
                in let b= f=s.ctor
            in  if b = false
                then (StringMap.add s.sname
                    {ss = s.ss;sname = s.sname; decls = s.decls; ctor = f; methods = s.methods} m , false)
                else
                    raise(Failure("There already exists a constructor called " ^f.fname ^" in struct " ^s.sname))
          | Method -> let s = try StringMap.find f.owner m
                    with Not_found -> raise (Failure ("method of undefined struct: " ^ f.owner^"::"^f.fname))
                in let b= List.mem f s.methods
            in
                    if b = false
                    then
                    (StringMap.add s.sname
                    {ss = s.ss;sname = s.sname; decls = s.decls; ctor = s.ctor; methods = f::s.methods} m , false)
                else
                    raise(Failure("There already exists a function called " ^f.fname ^" in strufct " ^s.sname))
        in
        List.fold_left ( fun (m,l) f -> let (m, cond) = filter_func m f in
        if cond then (m, f::l) else (m, l) ) (structs, []) functions
    in
    { s = List.map (fun st -> StringMap.find st.sname structs) prog.s;
      f = List.rev funcs ; v = globals }

let check prog =
    (* Get the global variables and functions *)
    let globals = prog.v
    and functions = prog.f
    and structs = prog.s
in
    
        report_dup(fun n-> "Duplicate Function Name " ^n)(List.map (fun fd -> fd.fname)functions);

        report_dup(fun n-> "Duplicate Global Name " ^n)(List.map (fun (_,a,_) ->  a)globals);

        report_dup(fun n-> "Duplicate Struct Name " ^n)(List.map(fun st -> st.sname)structs); 

let built_in_fun = StringMap.add "printi"
{rettyp=Void; fname="printi";params=[(Int, "x",Value)];locals=[];body=[];typ=Func;owner="None"}
(StringMap.add"printf" {rettyp=Void; fname="printf";params=[(Float, "x",Value)];locals=[];body=[];typ=Func;owner="None";}
(StringMap.add "prints" {rettyp=Void; fname="printf";params=[(String, "x",Value)];locals=[];body=[];typ=Func;owner="None";}
(StringMap.add "addshape"{rettyp=Void;fname="addshape";params=[(UserType("DUMMY",ShapeType),"x",Value)];locals=[];body=[];typ=Func;owner="None";}
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
let function_decl s = try StringMap.find s function_decls
    with Not_found -> raise (Failure ("Unrecognized function " ^ s ^" did you forget to define it ?"))
in
(*
    let _ = try List.find (fun s-> s ="main") function_decls 
        with Not_found -> raise(Failure (" Need a main function"))
    *)
    let _ = function_decl "main"
             
in


let function_check func =


let symbol_list = List.fold_left(fun m(t,n,_)->StringMap.add n t m)
    StringMap.empty(globals)

    in
        let rev_list =  List.fold_left(fun m(t,n,_)->StringMap.add n t m)
        symbol_list(func.params)
            in
                let final_list =  List.fold_left(fun m(t,n,_)->StringMap.add n t m)
                rev_list(func.locals)
                in
                    report_dup(fun n-> "Duplicate Parameter Name " ^n ^"in " ^ func.fname)(List.map (fun (_,a,_) ->  a)func.params);
                    report_dup(fun n-> "Duplicate local Name " ^n ^ " in " ^ func.fname)(List.map (fun (_,a,_) ->  a)func.locals);

let scopes_list = [(final_list,LocalScope);(symbol_list,GlobalScope)]
        in
let ret_type s=
    try StringMap.find s final_list
    with Not_found -> raise(Failure("Undeclared variable " ^s))
in

let struct_name_list = List.fold_left(fun m usr -> StringMap.add usr.sname usr m)
    StringMap.empty(structs)
    in 

let get_member_funcs name = let st = try StringMap.find name struct_name_list
    with  Not_found -> raise(Failure("Could not find memeber func"))
    in List.fold_left(fun m s -> StringMap.add s.fname s m)
        StringMap.empty st.methods
in
let get_member_constr name = let st = StringMap.find name struct_name_list
    in st.ctor
in


let get_struct_memember_var name = let st = try StringMap.find name struct_name_list
    with  Not_found -> raise(Failure("Could not find memeber func"))
    in List.fold_left(fun m (t,n) -> StringMap.add n t m)
        StringMap.empty st.decls
in

let list_of_member_var name var =
    let temp = get_struct_memember_var name
in
    StringMap.find var temp 

in


 let get_list_var name = 
        let x= try StringMap.find name struct_name_list
                with Not_found -> raise(Failure(" Could not find struct " ^name ^ " did you forgot to create  it?" ))
        in 
        List.fold_left(fun m (t,n)-> StringMap.add n t m)
        StringMap.empty(x.decls)
in

let check_struct_var name var = 
    let temp  = get_list_var name 
in  try StringMap.find var temp
    with Not_found -> raise(Failure ("Cannot find local variable " ^ var ^" in "^ name))
in

let check_mem_func_name name func =
    let temp = get_member_funcs name 
in try StringMap.find func temp
    with Not_found -> raise(Failure(func ^ " is not a member function of " ^name))
    in

let add_block_to_scope decls scope =
        let x = List.fold_left(fun m (t,n,_)  -> StringMap.add n t m)
                StringMap.empty (decls)
            in (x,LocalScope)::scopes_list
in
let print_scope  scope = 
    List.iter(fun s -> let smap = fst s
    in StringMap.iter( fun s n-> print_endline s) smap
    ) (List.rev scope)

in
let rec expr_b = function
    IntLit _-> Int
    |CharLit _-> Char
    |StringLit _-> String
    |FloatLit _ -> Float
    |VecLit (_,_)-> Vec
    |Id s -> if(func.typ=Method||func.typ=Constructor) (*ASSUMES THAT YOU ARE NOT USING SAME VAR NAME AS PARAMETERS*)
            then try list_of_member_var func.owner s
                with Not_found -> ret_type s 
        else
            ret_type s
    |Binop(e1,op,e2) as e -> let e1' = expr_b e1 and e2'=expr_b e2 in
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
        | _-> raise(Failure ("No unariy operator defined for "^ string_of_expr e1 ^ " "^string_of_expr e2))
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
    | _ -> raise(Failure("No unariy operator defined for "^ string_of_expr e1 ))
    )
    |Noexpr -> Void
    |Asnop(e1,asnp,e2)  -> let e1' = expr_b e1 and  e2'=expr_b e2 in 
    (match asnp with
    Asn when e1'=e2' ->  e1'
    |Asn when e2'=Void -> e1' (*Extermely poor idea but need to figure out constructor problem that return void*)
    |Asn when e1'=Float && e2'=Int ->  Float
    |Asn when e1'=Int && e2'=Float ->  Float
    |CmpAsn b when e1'=e2' ->  e1'
    |CmpAsn b when e1'=Float && e2'=Int ->  Float
    |CmpAsn b when e1'=Int && e2'=Float ->  Float
    | _ -> raise (Failure ("Invalid assigment of " ^ string_of_typ e1' ^ " to "^string_of_typ e2'))
    )
    |Call(e1, actuals) as call -> let e1' = (match e1 with 
        Id s -> let s' = try get_member_constr s 
                        with Not_found -> function_decl s
        in s'
        |Member (e,s) -> let e'= expr_b e in let
            e''= (match e' with
            UserType(s,e1) -> s
            | _-> raise(Failure("Not a UserType"))
            )
            in check_mem_func_name e'' s 

        |_-> raise(Failure("here"))
        )
in
        let fd = e1' in
        if fd.fname = "drawpoint" && func.fname<>"draw"
        then raise(Failure("Cannot have draw point in none draw method"))
    else
        if List.length actuals != List.length fd.params then
            raise (Failure ("Incorrect number of arguments "))
        else
            List.iter2 (fun (ft, _,_) e -> let et = expr_b e in
                ignore (check_ass ft et
                    (Failure ("Illegal actual argument found " ^ string_of_typ ft ^ " "^string_of_typ et))))
                fd.params actuals;
            fd.rettyp
    |Vecexpr (e1,e2) -> Vec
    |Posop (p,e2)-> let e2'=expr_b e2
    in (match p with
        Postinc when e2'=Float -> Float
        |Postinc when e2'=Int -> Int
        |Postdec when e2'=Float -> Float
        |Postdec when e2'=Int -> Int
        |_ -> raise(Failure("Cannot apply PostInc or PostDec " ^ " to" ^ string_of_expr e2))
    )
    |Trop(t,e1,e2,e3) -> Void
    |Index(e1,e2) -> let e1' = expr_b e1 and e2' = expr_b e2
                    in let te1' = (match e1' with
                     Array(e1,e2)->e1
                    | _-> raise(Failure("Not an array"))
                        )
                    in 
                    if e2'!= Int
                        then raise(Failure ("Must index with an integer "))
                         else te1'  
    |Member(e1,e2) -> let e1' = expr_b e1
in let te1'= (match e1' with
        UserType(s,e1) -> s
        |_ -> raise(Failure("Not a UserType"))
        )
    in
    check_struct_var te1' e2            
in 
let check_bool_expr e = if expr_b e != Int (*Could take in any number need to force check 1 or 0*)
    then raise(Failure(" Not a boolean value"))
else() in 
let rec stmt = function
    Block (_,e1)  -> let rec check_block = function
    [Return _ as ret] -> stmt ret
    |Return _ :: _ -> raise(Failure("Can't put more code after return"))
    |Block(s,e1):: ss ->  print_scope (add_block_to_scope s scopes_list ) ; check_block (e1 @ ss ) 
    |s :: ss -> stmt s; check_block ss  
    |[] -> ()
in check_block e1
    |Expr e -> ignore(expr_b e)
    |Return e -> let e1' = expr_b e in if e1'= func.rettyp then () else
        raise(Failure("Incorrect turn type on " ^ func.fname))
    |If(p,e1,e2) -> check_bool_expr p; stmt e1; stmt e2;
    |For(e1,e2,e3,state) -> ignore(expr_b e1); check_bool_expr e2; ignore(expr_b e3); stmt state
    |While(p,s) -> check_bool_expr p; stmt s 
    |ForDec (vdec,e1,e2,st1) -> ()
    |Timeloop(s1,e1,s2,e2,st1) ->()
    |Frameloop (s1,e1,s2,e2,st1)-> ()
    |Drawpoint (e1,e2)-> ()
    |Addshape e-> ()
    |_-> raise(Failure ("Here"))   
in stmt (Block (func.locals,func.body)) 
in
    List.iter function_check functions;
    List.iter (fun sdecl -> List.iter function_check (sdecl.ctor::sdecl.methods)) structs
    (* Need to somehow take care of "struct scope" once this is done it should mostly work*)