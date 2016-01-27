
let sl = "
(if (filemask (\"goodmask\")) (notify \"test message\"))"

let ast =
  let sl = SlParser.of_string sl in
  SlParser.t_to_ast sl

let optimized = ASTOptimized.of_ast ast

let main =
  let open ASTOptimized in
  let (context_info, optimized) = optimized in
  let filename = "test goodmask" in
  ASTOptimized.apply optimized context_info filename

let () =
  Lwt_main.run main
