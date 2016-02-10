
let register_regexp ~id ~fname ~flags args =
  let args = String.concat " | " args in
  Printf.eprintf "register regexp id=%i, fn=%s, args=%s\n" id fname args
    

let () = Ast_mapper.register "getenv"
    (fun args -> AstMapper.my_mapper register_regexp)
