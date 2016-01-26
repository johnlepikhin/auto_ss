
open Sl

let ast =
  let open AST in
  If (Filemask ["goodmask1"; "goodmask"],
      (SetContext
         ("otherfile",
          (If (Filemask ["other"],
           Notify "OK")))))

let optimized = Optimized.of_ast ast

let () =
  let open Optimized in
  let (context_info, optimized) = optimized in
  let filename = "test goodmask" in
  Optimized.apply optimized context_info filename
