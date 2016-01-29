
let ast =
  let open AST in
  If (Filemask ([`CASELESS], ["Goodmask1"; "Goodmask"]),
      (SetContext
         ("otherfile",
          (If (Filemask ([], ["other"]),
           Notify "OK")))))

let optimized = ASTOptimized.Parser.of_ast ast

let main =
  let open ASTOptimized.Sample in
  let (context_info, optimized) = optimized in
  let filename = "test goodmask" in
  ASTOptimized.Sample.apply optimized context_info filename filename
