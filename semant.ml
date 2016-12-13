open Ast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns possibly modified Ast if successful,
   throws an exception if something is wrong. *)
let report_dup  exceptf list =
        let rec helper = function 
            n1 :: n2 ::_ when n1=n2 && n1 <> "draw" -> raise (Failure(exceptf n1) )
            |_ :: t -> helper t
            |[]->()

        in helper(List.sort compare list)

let struct_build prog =
    let globals = prog.v
    and functions = prog.f 

    in  
    (* A map of all struct/shape types *) (* This ignores duplicate struct: handle duplicates CHECK IF ALREADY IN MAP*)
    let structs = List.fold_left ( fun m st -> StringMap.add st.sname st m)
                StringMap.empty prog.s in
    let (structs,funcs) = (* Refers to structs and non-member functions *)
        (* Puts methods and constructors with appropriate struct and returns tuple
            (map, bool) *)
        let filter_func m f =
          match f.typ with
            Func -> (m, true) (* true means keep function *)
          | Constructor -> let s = try StringMap.find f.owner m (* This ignores duplicate constructor: handle duplicates CHECK IF ALREADY IN MAP*)
                    with Not_found -> raise (Failure ("constructor of undefined struct: " ^ f.owner^"::"^f.fname))
                in (StringMap.add s.sname
                    {ss = s.ss;sname = s.sname; decls = s.decls; ctor = f; methods = s.methods} m , false)
          | Method -> let s = try StringMap.find f.owner m (* This ignores duplicat method: handle duplicates CHECK IF ALREADY IN MAP*)
                    with Not_found -> raise (Failure ("method of undefined struct: " ^ f.owner^"::"^f.fname))
                in (StringMap.add s.sname
                    {ss = s.ss;sname = s.sname; decls = s.decls; ctor = s.ctor; methods = f::s.methods} m , false)
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


    let function_decls =
        List.map(fun fd -> fd.fname) functions
    
in
    let _ = try List.find (fun s-> s ="main") function_decls 
        with Not_found -> raise(Failure (" Need a main function"))
in
(*
int x;
int main()
{
    int y,z;
    {
        int z;
    }
}
*)
(*[ ('z', LocalScope); ('y,z', LocalScope);('x',GlobalScope)]*)

let function_check func =
    (*let rec block_check (var_list, stmt_list) scopes([ (map3, LocalScope) ; (map, LocalScope) ; (map, GlobalScope)]*)

    let symbol_list = 
     report_dup(fun n-> "Duplicate Parameter Name " ^n ^"in " ^ func.fname)(List.map (fun (_,a,_) ->  a)func.params);
     report_dup(fun n-> "Duplicate local Name " ^n ^ " in " ^ func.fname)(List.map (fun (_,a,_) ->  a)func.locals);
    List.fold_left(fun m(t,n,_)->StringMap.add n t m)
        StringMap.empty (globals)
    in
    let rev_list =  List.fold_left(fun m(t,n,_)->StringMap.add n t m)
        symbol_list (func.params)
    in
    let final_list =  List.fold_left(fun m(t,n,_)->StringMap.add n t m)
        rev_list (func.locals)
    in
    let ret_type s= StringMap.find s symbol_list
    in ()

in
List.iter function_check functions; (*;
    (* check methods and constructors for each struct *)
    List.iter (fun sdecl -> List.iter function_check (sdecl.A.ctor::sdecl.A.methods)) structs;
    the_module *)