
open Lwt

let configs = [
  "sldir:configs";
  "slarg"
]

let main =
  Config.get configs
  >>= fun (context_info, optimized) ->
  let filename = "tests/matchedfile" in
  ASTOptimized.apply optimized context_info filename;
  return ()
  
let () =
  Lwt_main.run main
