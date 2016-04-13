
open Lwt

let jobs = ref 2

let command = ref "ss"

let usage = ArgPipeFormat.usageInOut ^ "\nCOMMAND LINE ARGUMENTS:\n"

let args = ArgPipeFormat.argsInOut @ [
    "-j", Arg.Set_int jobs, "Jobs (default 2)";
    "-cmd", Arg.Set_string command, "Command to run (default 'ss'). Correct -if and -of will be added automatically"
  ]

let () =
  Arg.parse args (fun _ -> ()) usage;
  if !jobs < 1 then
    failwith "Invalid -j argument value"

module IN_FORMAT = (val !ArgPipeFormat.input_format)
module OUT_FORMAT = (val !ArgPipeFormat.output_format)
module P = PipeLwt.Make (PipeFmtMain.Type) (IN_FORMAT) (OUT_FORMAT)

type process = {
  io : P.io;
  process : Lwt_process.process;
  process_reader : unit Lwt.t;
}

let main =
  let pipe = P.init Lwt_io.stdin Lwt_io.stdout in
  let command = Printf.sprintf "%s -if %s -of %s" !command P.in_format_name P.out_format_name |> Lwt_process.shell in
  let pool = Array.init !jobs (fun _ ->
      let process = Lwt_process.open_process command in
      let io = P.init process#stdout process#stdin in
      let process_reader =
        P.iter_input
          (P.output pipe)
          io
      in
      { io; process; process_reader }
    )
  in
  let select_send =
    let id = ref 0 in
    fun v ->
      incr id;
      if !id + 1 > Array.length pool then id := 0;
      let p  = Array.get pool !id in
      P.output p.io v
  in
  P.iter_input select_send pipe
  >>= fun () ->
  let lpool = Array.to_list pool in
  Lwt_list.iter_s (fun p -> Lwt_io.close p.io.P.oc) lpool
  >>= fun () ->
  let threads = List.map (fun p -> p.process_reader) lpool in
  Lwt.join threads

let () =
  Lwt_main.run main

