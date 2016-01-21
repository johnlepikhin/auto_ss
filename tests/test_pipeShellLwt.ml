
module Pipe = PipeLwt.Make (PipeShell)

let printer r =
  Pipe.output Lwt_io.stdout r

let main =
  Pipe.iter_input printer Lwt_io.stdin

let () =
  Lwt_main.run main
