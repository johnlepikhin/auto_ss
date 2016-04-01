
open Lwt
open Cohttp
open Cohttp_lwt_unix

let port = ref 800

module type ENV =
sig
  val compile_path : string ref
end

module Env_UNIX : ENV =
struct
  let compile_path = ref "/tmp/ss_compile_farm"
end

module Env =
  (val (
     let open Sys in
     match os_type with
     | "Unix" -> (module Env_UNIX : ENV)
     | _ -> failwith "Unsupported OS"
   )
  )

let args = Arg.[
    "-p", Set_int port, "TCP port to listen. Default 800";
    "-d", Set_string Env.compile_path, ("Path to compile cache. Default: " ^ !Env.compile_path)
  ]


let readfile filename =
  let open Unix in
  try
    let stat = stat filename in
    let readsize = stat.st_size in
    let buf = Buffer.create readsize in
    let ch = open_in filename in
    try
      Buffer.add_channel buf ch readsize;
      close_in ch;
      Some (Buffer.contents buf)
    with
    | _ ->
      close_in ch;
      None
  with
  | _ -> None

let savefile filename content =
  let ch = open_out filename in
  output_string ch content;
  close_out ch

let send_cmxs cmxs =
  let (body, code) =
    let body = readfile cmxs in
    match body with
    | Some body -> body, 200
    | None -> "Script compiled successfully but result cannot be read", 410
  in
  Server.respond_string ~status:(`Code code) ~body ()

let compile fingerprint compile_path src =
  Sys.chdir compile_path;
  let script_name = Printf.sprintf "Script_%s" fingerprint in
  savefile (Printf.sprintf "%s.ml" script_name) src;
  Printf.sprintf "ocamlbuild -use-ocamlfind -tag 'package(ss.script)' -tag 'package(ss.script.ppx)' -tag 'package(pcre)' %s.cmxs" script_name
  |> Sys.command
  |> fun res ->
  match res with
  | 0 ->
    Printf.sprintf "_build/%s.cmxs" script_name
    |> send_cmxs 
  | _ ->
    let body = readfile "_build/_log" in
    let (body, code) =
      match body with
      | Some body -> body, 202
      | None -> "Script compiled with errors and error log cannot be read", 404
    in
    Server.respond_string ~status:(`Code code) ~body ()

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
      (try Unix.mkdir compile_path 0o700 with | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      compile fingerprint compile_path src
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
    match meth, uri_path with
    | "POST", "/compile" ->
      body |> Cohttp_lwt_body.to_string >>= check_cache ~version
    | _ ->
      Server.respond_string ~status:(`Code 404) ~body:"Not found" ()
  in
  Server.create ~mode:(`TCP (`Port !port)) (Server.make ~callback ())

let () =
  Arg.parse args (fun _ -> ()) ""

let () = ignore (Lwt_main.run (server ()))
