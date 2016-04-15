open Printf

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (globals, functions) = 
    let context = L.global_context () in
    let the_module = L.create_module context "MicroC"
    and i32_t = L.i32_type context
    and i8_t = L.i8_type context
    and i1_t = L.i1_type context
    and void_t = L.void_type context in

    let ltype_of_type = function
        A.Int -> i32_t
        | A.Bool -> i1_t
        | A.Void -> void_t in

    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func = L.declare_function "printf" printf_t the_module in

    let fty = L.function_type i32_t [| |] in
  (*  let f = L.define_function "main" fty the_module in *)
(*
    let llbuilder = L.builder_at_end context (L.entry_block f) in
    let _ = L.build_ret (L.const_int i32_t 0) llbuilder in

     the_module ;  *)
    (*let build_function_body fdecl = 
        let the_function = L.define_function "print" 
*)
  (*
    let llbuilder = L.builder_at_end context (L.entry_block f) in
        let int_format_str = L.build_global_stringptr "%d\n" "fmt" llbuilder in
        let _ = L.build_call printf_func [| int_format_str; L.const_int i8_t 69 |] "printf" llbuilder in
        let _ = L.build_ret (L.const_int i32_t 3) llbuilder in
    the_module;*)
    let function_decls =
        let function_decl m fdecl =
        let name = fdecl.A.fname
        and formal_types = 
        Array.of_list (List.map (fun (t, _) -> ltype_of_type t) fdecl.A.formals) in
        let ftype = L.function_type (ltype_of_type fdecl.A.typ) formal_types in
        StringMap.add name (L.define_function name ftype the_module, fdecl) m in

        List.fold_left function_decl StringMap.empty functions in


    let build_function_body fdecl =
       let (the_function,_) = StringMap.find fdecl.A.fname function_decls in
       let builder = L.builder_at_end context (L.entry_block the_function) in
        let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

        let rec expr builder = function
            A.Literal i -> L.const_int i32_t i 
         |   A.Call ("print", [e]) -> printf "%s " "expr:call "; L.build_call printf_func [| int_format_str; (expr builder e) |] "printf" builder 
        | A.Call (f, act) -> 
            let (fdef, fdecl) = StringMap.find f function_decls in
            let actuals = List.rev (List.map (expr builder) (List.rev act)) in
            let result = (match fdecl.A.typ with 
                            A.Void -> ""
                            | _ -> f ^ "_result") in
        L.build_call fdef (Array.of_list actuals) result builder
        in

        let rec stmt builder = function
            A.Block sl -> printf ";%s" "block "; List.fold_left stmt builder sl
        | A.Expr e -> printf "%s" "expr "; ignore (expr builder e); builder
        | A.Return e -> printf "%s" "return "; builder
        | _ -> printf "%s" "nothing "; builder in
        let builder = stmt builder (A.Block fdecl.A.body) in
       let _ = L.build_ret (L.const_int i32_t 3) builder in
        (*printf "%s " fdecl.A.fname in*)

    let rec expr builder = function
        A.Call ("print", [e]) -> printf "%s " "expr:print"; L.build_call printf_func [| int_format_str; L.const_int i8_t 666 |] "printf" builder
        | _ -> printf "%s " "expr:nothing"; L.const_int i8_t 777 in
    () in
    
    List.iter build_function_body functions;
    the_module
