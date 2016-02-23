
open MonitorFileUpdates

let path = [
  Path "/var";
  groups_of_regexp "%logdir%$";
  groups_of_regexp "%logfile%\\.log$";
]

let get () =
  getfiles path

let () =
  let l1 = get () in
  print_string "Hit enter when /var/log/*.log is updated...";
  ignore (read_line ());
  let l2 = get () in
  let diff = getdiff l1 l2 in
  List.iter (fun info ->
      let values = List.map (fun (k,v) -> Printf.sprintf "%s=%s" k v) info.values |> String.concat ", " in
      Printf.printf "file %s changed, begin=%Li, end=%Li, values: %s\n" info.path info.pos_begin info.pos_end values
    ) diff
