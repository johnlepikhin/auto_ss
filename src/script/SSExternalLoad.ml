
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
  Lwt.catch
    (fun () ->
       try
         let raise_exn = ref true in
         let t_getter =
           Printf.sprintf "http://%s/compile?ssscript_version=%i" server SSScript.version
           |> Uri.of_string
           |> Client.post ~body
           >>= fun (response, body) ->
           let status = Response.status response in
           match status with
           | `OK ->
             Cohttp_lwt_body.to_string body
             >>= fun body ->
             raise_exn := false;
             Lwt.return body
           | `Internal_server_error ->
             raise_exn := false;
             cfail ~fatal:false "Compile farm internal error"
           | `Gone ->
             raise_exn := false;
             cfail ~fatal:false "Cannot read result library"
           | `Not_found ->
             raise_exn := false;
             cfail ~fatal:false "Compilation failed and cannot read error log"
           | `Accepted ->
             Cohttp_lwt_body.to_string body
             >>= fun error ->
             raise_exn := false;
             let msg = Printf.sprintf "Failed to compile: %s" error in
             cfail ~fatal:true msg
           | _ ->
             raise_exn := false;
             cfail ~fatal:false "Other HTTP error"
         in
         let t_timeout =
           Lwt_unix.sleep 20.
           >>= fun () ->
           if !raise_exn then
             cfail ~fatal:false "Timeout"
           else
             Lwt.return ""
         in
         t_getter <?> t_timeout
       with
       | e -> Lwt.fail e
    )
    (function
      | Unix.Unix_error _ -> cfail ~fatal:false "Cannot get compiled module from compile farm"
      | exn -> Lwt.fail exn
    )

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
      Lwt.catch
        (fun () ->
           get_cmxs server script
           >>= fun data ->
           let file = save_cmxs ~temp_dir data in
           load_cmxs file
           >>= fun () ->
           if Sys.os_type <> "Win32" then
             Sys.remove file;
           Lwt.return ()
        )
        (function
          | (CompileError (_, true)) as exn ->
            Lwt.fail exn
          | CompileError (msg, false) ->
            Printf.eprintf "Compilation error:\n\n%s\n" msg;
            try_server tl
          | exn ->
            Lwt.fail exn
        )
  in
  try_server farm
