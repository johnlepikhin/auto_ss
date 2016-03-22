
open Lwt

let configs = [
  "dir:configs";
  "arg"
]

let notify_cb fileinfo message =
  Printf.printf "Matched rule: %s\n" message

let queuefile_cb fileinfo filename =
  Printf.printf "Queue file: %s\n" filename

let main =
  SSExternalLoad.load_cmxs "./_build/tests/test_config_plugin.cmxs"
  >>= fun () ->
  
  let filename = "tests/matchedfile.php" in
  SSScript.External.set_notify notify_cb;
  SSScript.External.set_queuefile queuefile_cb;
  let () = SSExternals.run filename in
  return ()
  
let () =
  Lwt_main.run main
