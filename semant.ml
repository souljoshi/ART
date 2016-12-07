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


let check prog =
    (* Get the global variables and functions *)
    let globals = prog.v
    and functions = prog.f in

    
    (*report_dup(fun n-> "Duplicate Function Name " ^n)(List.map (fun fd -> fd.fname)functions);*)

    report_dup(fun n-> "Duplicate Global Name " ^n)(List.map (fun (_,a,_) ->  a)globals);

    let function_decls =
        List.map(fun fd -> fd.fname) functions
    in

    let _ = try List.find (fun s-> s ="main") function_decls 
        with Not_found -> raise(Failure (" Need a main function"))
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
in
  List.iter function_check functions;




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
                in (StringMap.add s.sname
                    {ss = s.ss;sname = s.sname; decls = s.decls; ctor = f; methods = s.methods} m , false)
          | Method -> let s = try StringMap.find f.owner m
                    with Not_found -> raise (Failure ("method of undefined struct: " ^ f.owner^"::"^f.fname))
                in (StringMap.add s.sname
                    {ss = s.ss;sname = s.sname; decls = s.decls; ctor = s.ctor; methods = f::s.methods} m , false)
        in
        List.fold_left ( fun (m,l) f -> let (m, cond) = filter_func m f in
        if cond then (m, f::l) else (m, l) ) (structs, []) functions
    in
    { s = List.map (fun st -> StringMap.find st.sname structs) prog.s;
      f = List.rev funcs ; v = globals }


  

  