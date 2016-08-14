
open Lwt
open Cohttp
open Cohttp_lwt_unix

let port = ref 8999

module type ENV =
sig
  val compile_path : string ref
end

module Env_UNIX : ENV =
struct
  let compile_path = ref "/tmp/ss_compile_farm"
end

module Env_Windows : ENV =
struct
  let compile_path = ref "c:/tmp/ss_compile_farm"
end

module Env =
  (val (
     let open Sys in
     match os_type with
     | "Unix" -> (module Env_UNIX : ENV)
     | "Win32" -> (module Env_Windows : ENV)
     | _ -> failwith "Unsupported OS"
   )
  )

let args = Arg.[
    "-p", Set_int port, "TCP port to listen. Default 8999";
    "-d", Set_string Env.compile_path, ("Path to compile cache. Default: " ^ !Env.compile_path)
  ]

let readfile filename =
  let open Unix in
  try
    let stat = stat filename in
    let readsize = stat.st_size in
    let buf = Buffer.create readsize in
    let ch = open_in_bin filename in
    try
      let rbuf = Bytes.create 4096 in
      let rec loop pos =
        let rd = input ch rbuf 0 4096 in
        if rd = 0 then
          ()
        else (
          Buffer.add_substring buf rbuf 0 rd;
          loop (pos+rd)
        )
      in
      loop 0;
      close_in ch;
      Some (Buffer.contents buf)
    with
    | Unix.Unix_error (err, _, _) ->
      let errmsg = Unix.error_message err in
      Printf.eprintf "Cannot read CMXS: %s\n" errmsg;
      close_in ch;
      None
  with
  | Unix.Unix_error (err, _, _) ->
    let errmsg = Unix.error_message err in
    Printf.eprintf "Cannot open CMXS: %s\n" errmsg;
    None
  | exn ->
    Printf.eprintf "Cannot open/read CMXS: %s\n" (Printexc.to_string exn);
    None

let send_cmxs cmxs =
  let (body, code) =
    let body = readfile "/etc/hostname" in
    match body with
    | Some body -> body, 200
    | None ->
      let msg = "Script compiled successfully but result cannot be read" in
      Printf.eprintf "Error: %s\n" msg;
      msg, 410
  in
  Server.respond_string ~status:(`Code code) ~body ()

(*
let compile fingerprint compile_path src =
  Sys.chdir compile_path;
  let script_name = Printf.sprintf "Script_%s" fingerprint in
  savefile (Printf.sprintf "%s.ml" script_name) src;
  Printf.sprintf "ocamlfind ocamlopt -verbose -shared -package ss.script -package ss.script.ppx -package pcre -o %s.cmxs %s.ml >./_log 2>&1" script_name script_name
  |> Sys.command
  |> fun res ->
  match res with
  | 0 ->
    Printf.sprintf "%s.cmxs" script_name
    |> send_cmxs 
  | _ ->
    let body = readfile "_log" in
    let (body, code) =
      match body with
      | Some body -> body, 202
      | None ->
        let msg = "Script compiled with errors and error log cannot be read" in
        Printf.eprintf "Error: %s\n" msg;
        msg, 404
    in
    Server.respond_string ~status:(`Code code) ~body ()
*)

let check_cache ~version src =
  let open Cryptokit in
  let fingerprint = Printf.sprintf "%s %s" version src |> hash_string (Hash.sha224 ()) |> transform_string (Hexa.encode ()) in
  try
    (try Unix.mkdir !Env.compile_path 0o700 with | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    let compile_path = Filename.concat !Env.compile_path fingerprint in
    let cmxs = Filename.(concat compile_path (concat "_build" (Printf.sprintf "Script_%s.cmxs" fingerprint))) in
    if Sys.file_exists cmxs then (
      send_cmxs cmxs
    ) else (
      send_cmxs cmxs
        (*
      (try Unix.mkdir compile_path 0o700 with | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      compile fingerprint compile_path src
*)
    )
  with
  | _ -> Server.respond_string ~status:(`Code 500) ~body:"Internal error" ()

let server () =
  let callback _conn req body =
    let uri = Request.uri req in
    let uri_path = Uri.path uri in
    let meth = req |> Request.meth |> Code.string_of_method in
    let version =
      match Uri.get_query_param uri "ssscript_version" with
      | None -> "0"
      | Some v -> v
    in
    let r =
      match meth, uri_path with
      | "POST", "/compile" ->
        body |> Cohttp_lwt_body.to_string >>= check_cache ~version
      | _ ->
        Server.respond_string ~status:(`Code 404) ~body:"Not found" ()
    in
    flush_all ();
    r
  in
  Server.create ~mode:(`TCP (`Port !port)) (Server.make ~callback ())

let () =
  Arg.parse args (fun _ -> ()) ""

let () = ignore (Lwt_main.run (server ()))
