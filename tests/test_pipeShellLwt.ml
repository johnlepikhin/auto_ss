
module Pipe = PipeLwt.Make (PipeFmtMain.Type) (PipeShell.Make) (PipeShell.Make)

let pipe = Pipe.init Lwt_io.stdin Lwt_io.stdout

let printer r =
  Pipe.output pipe r

let main =
  Pipe.iter_input printer pipe

let () =
  Lwt_main.run main
