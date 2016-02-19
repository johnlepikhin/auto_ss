
module Pipe = PipeLwt.Make (PipeFmtMain.Type) (PipeShell.Make) (PipeShell.Make)

let printer r =
  Pipe.output Lwt_io.stdout r

let main =
  Pipe.iter_input printer Lwt_io.stdin

let () =
  Lwt_main.run main
