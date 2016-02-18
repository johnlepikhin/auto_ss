
open MonitorFileUpdates

let path = [
  Path "/var";
  Path "log";
  Regexp (Pcre.regexp "\\.log$")
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
  Collection.iter (fun path (st, pos_begin, pos_end) ->
      Printf.printf "file %s changed, begin=%Li, end=%Li\n" path pos_begin pos_end
    ) diff
