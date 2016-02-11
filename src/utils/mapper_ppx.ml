
let generator =
  let id = ref 0 in
  fun () ->
    incr id;
    !id

let register_regexp ~fname ~flags args =
  let id = generator () in
  let args = String.concat " | " args in
  Printf.eprintf "register regexp id=%i, fn=%s, args=%s\n" id fname args;
  id

let () = Ast_mapper.register "getenv"
    (fun args -> AstMapper.my_mapper register_regexp)
