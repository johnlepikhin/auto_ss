
let ast =
  let open AST in
  If (Filemask ["goodmask1"; "goodmask"],
      (SetContext
         ("otherfile",
          (If (Filemask ["other"],
           Notify "OK")))))

let optimized = ASTOptimized.of_ast ast

let () =
  let open ASTOptimized in
  let (context_info, optimized) = optimized in
  let filename = "test goodmask" in
  ASTOptimized.apply optimized context_info filename
