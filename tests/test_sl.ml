
let sl = "
(seq
  (if (filemask \"goodmask\") (notify \"test message1\"))
  (if (filemask \"goodmask\") (notify \"test message2\"))
  (if (filemask \"goodmask\") (notify \"test message3\"))
  (if (filemask \"goodmask\") (notify \"test message4\"))
  (if (filemask \"goodmask\") (notify \"test message5\"))
  (if (filemask \"goodmask\") (notify \"test message6\"))
)"

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
