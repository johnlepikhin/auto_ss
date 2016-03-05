
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

let main =
  Arg.parse args (fun _ -> ()) usage;
  let module IN_FORMAT = (val !ArgPipeFormat.input_format) in
  let module OUT_FORMAT = (val !ArgPipeFormat.output_format) in
  let module P = PipeLwt.Make (PipeFmtMain.Type) (IN_FORMAT) (OUT_FORMAT) in
  let pipe = P.init Lwt_io.stdin Lwt_io.stdout in
  let command = Printf.sprintf "%s -if %s -of %s" !command P.in_format_name P.out_format_name |> Lwt_process.shell in
  let pool = Array.init !jobs (fun _ ->
      let process = Lwt_process.open_process command in
      let process_pipe = P.init process#stdout process#stdin in
      Lwt.async (fun () -> P.iter_input (P.output pipe) process_pipe);
      process_pipe
    )
  in
  let select_send =
    let id = ref 0 in
    fun v ->
      incr id;
      if !id + 1 > Array.length pool then id := 0;
      let process_pipe = Array.get pool !id in
      P.output process_pipe v
  in
  P.iter_input select_send pipe

let () =
  Lwt_main.run main
