
open Lwt

let configs = [
  "sldir:configs";
  "slarg"
]

let main =
  Config.get configs
  >>= fun (context_info, optimized) ->
  let filename = "tests/matchedfile" in
  ASTOptimized.Sample.apply optimized context_info filename
  
let () =
  Lwt_main.run main
