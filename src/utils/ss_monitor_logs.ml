
open MonitorFileUpdates

let mask = ref []

let sleep = ref 0

let args = Arg.[
    "-p", String (fun s -> mask := Path s :: !mask), "Add string element to path mask";
    "-r", String (fun s -> mask := Regexp (Pcre.regexp s) :: !mask), "Add regexp element to path mask";
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


let output files =
  Collection.iter (fun path (st, pos_begin, pos_end) ->
      List.map PipeHuman.escape [path; Int64.to_string pos_begin; Int64.to_string pos_end]
      |> String.concat "\t"
      |> print_endline
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
