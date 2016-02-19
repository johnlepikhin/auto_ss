
open MonitorFileUpdates

let mask = ref []

let sleep = ref 0

let args = Arg.[
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

"


let () =
  Arg.parse args (fun _ -> ()) usage;
  mask := List.rev !mask

module P = PipeUnix.Make (PipeFmtLog.Type) (PipeHuman.Make) (PipeHuman.Make)

let output files =
  Collection.iter (fun path (st, begin_pos, end_pos, values) ->
      if Int64.sub end_pos begin_pos > 0L then
        let record = PipeFmtLog.Type.{
            file = path;
            begin_pos;
            end_pos;
            values;
          }
        in
        P.output stdout (Pipe.Record record)
    ) files;
  flush stdout

let rec checkUpdates prev =
  Unix.sleep !sleep;
  let next = getfiles !mask |> statfiles in
  let diff = getdiff prev next in
  output diff;
  checkUpdates next

let () =
  let files = getfiles !mask |> statfiles in
  if !sleep == 0 then
    output files
  else
    checkUpdates files
