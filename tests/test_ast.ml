
let ast =
  let open AST in
  If (Filemask ([`CASELESS], ["Goodmask1"; "Goodmask"]),
      (SetContext
         ("otherfile",
          (If (Filemask ([], ["other"]),
           Notify "OK")))))

let optimized = ASTOptimized.of_ast ast

let main =
  let open ASTOptimized in
  let (context_info, optimized) = optimized in
  let filename = "test goodmask" in
  ASTOptimized.apply optimized context_info filename
