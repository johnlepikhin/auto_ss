
open Lwt

let configs = [
  "dir:configs";
  "arg"
]

let notify_cb fileinfo message =
  Printf.printf "Matched rule: %s\n" message

let main =
  SSConfig.get configs
  >>= fun scripts ->
  let script =
    List.map (fun (domain, script) -> (SSConfig_sig.string_of_domain domain), script) scripts
    |> SSScript.prepare
  in
  let filename = "tests/matchedfile" in
  let fileinfo = SSScript.fileinfo filename in
  let () = SSScript.run ~notify_cb ~script fileinfo in
  return ()
  
let () =
  Lwt_main.run main
