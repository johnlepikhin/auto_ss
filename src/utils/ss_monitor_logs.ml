
open MonitorFileUpdates

let mask = ref []

let sleep = ref 0

let args = ArgPipeFormat.argsOut @ Arg.[
    "-p", String (fun s -> mask := Path s :: !mask), "Add string element to path mask";
    "-r", String (fun s -> mask := groups_of_regexp s :: !mask), "Add regexp element to path mask";
    "-s", Set_int sleep, "Sleep for specified number of seconds before next check. If =0 then just output all found files";
  ]

let usage = "

Example usage:

* ss_monitor_logs -p '/var/log' -r '\\\\.log$' -s 30

Will check for changes in /var/log/*.log every 30 seconds

* ss_monitor_logs -p '/var/log' -r '\\\\.log$'

Will list all files matching mask /var/log/*.log

" ^ ArgPipeFormat.usageOut ^ "\nCOMMAND LINE ARGUMENTS:\n"

let () =
  Arg.parse args (fun _ -> ()) usage;
  mask := List.rev !mask;

  let module OUT_FORMAT = (val !ArgPipeFormat.output_format) in
  let module P = PipeUnix.Make (PipeFmtLog.Type) (OUT_FORMAT) (OUT_FORMAT) in

  let pipe = P.init stdin stdout in

  let output_info info =
    let module M = MonitorFileUpdates in
    if Int64.sub info.M.pos_end info.M.pos_begin > 0L then
      let record = PipeFmtLog.Type.{
          file = info.path;
          begin_pos = info.M.pos_begin;
          end_pos = info.M.pos_end;
          values = info.values;
        }
      in
      P.output pipe (Pipe.Record record)
  in

  let output_diff files =
    List.iter output_info files;
    flush stdout
  in

  let output_state state =
    Hashtbl.iter (fun _ info -> output_info info) state;
    flush stdout
  in

  let rec checkUpdates prev =
    Unix.sleep !sleep;
    let next = getfiles !mask in
    let diff = getdiff prev next in
    output_diff diff;
    checkUpdates next
  in

  let files = getfiles !mask in
  if !sleep == 0 then
    output_state files
  else
    checkUpdates files
