
type fatal = bool

exception CompileError of string * fatal

let cfail ~fatal msg =
  Lwt.fail (CompileError (msg, fatal))

let load_cmxs file =
  let open Dynlink in
  try
    adapt_filename file |> loadfile;
    Lwt.return ()
  with
  | Error msg ->
    let msg = error_message msg |> Printf.sprintf "Unable to load file %s\n" in
    cfail ~fatal:true msg

let get_cmxs server script : string Lwt.t =
  let open Lwt in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let body = Cohttp_lwt_body.of_string script in
  try%lwt
    try
      Printf.sprintf "http://%s/compile?ssscript_version=%i" server SSScript.version
      |> Uri.of_string
      |> Client.post ~body
      >>= fun (response, body) ->
      let status = Response.status response in
      match status with
      | `OK ->
        Cohttp_lwt_body.to_string body
      | `Internal_server_error ->
        cfail ~fatal:false "Compile farm internal error"
      | `Gone ->
        cfail ~fatal:false "Cannot read result library"
      | `Not_found ->
        cfail ~fatal:false "Compilation failed and cannot read error log"
      | `Accepted ->
        let%lwt error = Cohttp_lwt_body.to_string body in
        let msg = Printf.sprintf "Failed to compile: %s" error in
        cfail ~fatal:true msg
      | _ ->
        cfail ~fatal:false "Other HTTP error"
    with
    | e -> Lwt.fail e
  with
    | Unix.Unix_error _ -> cfail ~fatal:false "Cannot get compiled module from compile farm"

let save_cmxs ~temp_dir data =
  let (filename, ch) = Filename.open_temp_file ~temp_dir ~mode:[Open_wronly; Open_binary; Open_creat; Open_excl] "SScriptGen" ".cmxs" in
  Unix.chmod filename 0o600;
  output_string ch data;
  close_out ch;
  filename

let load_remote ~farm ~temp_dir script =
  let open Lwt in
  let rec try_server = function
    | [] ->
      cfail ~fatal:true "All servers of compile farm are failed"
    | server :: tl ->
      try%lwt
        let%lwt data = get_cmxs server script in
        let file = save_cmxs ~temp_dir data in
        let%lwt () = load_cmxs file in
        Sys.remove file;
        Lwt.return ()
      with
      | (CompileError (_, true)) as exn ->
        Lwt.fail exn
      | CompileError (_, false) ->
        try_server tl
  in
  try_server farm
