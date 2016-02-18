
open MonitorFileUpdates

let path = [
  Path "/var";
  groups_of_regexp "%logdir%$";
  groups_of_regexp "%logfile%\\.log$";
]

let get () =
  getfiles path
  |> statfiles

let () =
  let l1 = get () in
  print_string "Hit enter when /var/log/*.log is updated...";
  ignore (read_line ());
  let l2 = get () in
  let diff = getdiff l1 l2 in
  Collection.iter (fun path (st, pos_begin, pos_end, values) ->
      let values = List.map (fun (k,v) -> Printf.sprintf "%s=%s" k v) values |> String.concat ", " in
      Printf.printf "file %s changed, begin=%Li, end=%Li, values: %s\n" path pos_begin pos_end values
    ) diff
