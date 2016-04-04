
let configReaders = ref [
  "arg";
]

let compile_farm = ref [
    "127.0.0.1:800"
  ]

let temp_dir = ref "/tmp"

let set_compile_farm =
  let rex = Pcre.regexp "," in
  fun s ->
    compile_farm := Pcre.split ~rex s

let args = ArgPipeFormat.argsInOut @ Arg.[
    "-r", String (fun s -> configReaders := s :: !configReaders), "Add config new reader";
    "-I", String (fun _ -> ()), "Inline check with specified condition";
    "-c", String set_compile_farm, "Location of compile farm. For example: 127.0.0.1:80,farm.localnet:8080 (default 127.0.0.1:800)";
    "-t", Set_string temp_dir, "Directory to store temporary files. Must be mounted with exec enabled!";
]

let usage = "
TYPICAL USAGE

Fast check some rule on all files in /var/www:

find /var/www -type f | ss -I 'bodymask () \"substring1\" \"substring2\")'

All rules are described using Ocaml syntax. There are different ways to
pass configs to SS:

 * 'file:/path/to/file1:/path/to/file2:...'
 * 'dir:/path/to/dir1:/path/to/file2:...'

Current template for new configs:

==============================================
open SSScript.External

SSExternals.register (module (struct
  let check context =
    ...
end))
==============================================

Function 'notify' notifies scanner about issue in the context:

notify context \"Virus Win32.CIH\"

For example, notify about Win32.CIH in ALL files:

SSExternals.register (module (struct
  let check context =
    notify context \"Virus Win32.CIH\"
end))

To match file names and content some special syntax available:

Return true if file name matches *.php:
filemask () \"\\\\.php$\"

Return true if file name matches *.php, *.PHP, ...:
filemask (i) \"\\\\.php$\"

Return true if file content matches 'aaa':
bodymask () \"aaa\"

Return true if file content matches 'privet' (UTF-8) in any russian charset:
rusbodymask \"privet\"

For example:

SSExternals.register (module (struct
  let check context =
    if rusbodymask \"privet\" then
      notify context \"Russian hello\"
end))

To make rules description simplier, use shortcut:

rule context \"Virus Win32.CIH\" (<CONDITION>)

For example:

SSExternals.register (module (struct
  let check context =
    rule context \"Russian hello\" (rusbodymask \"privet\")
end))

One may specify as many checks as required separating it by semicolon:

SSExternals.register (module (struct
  let check context =
    rule context \"Russian hello\" (rusbodymask \"privet\");
    rule context \"English hello\" (bodymask (i) \"hello\");
    rule context \"Mongolian hello\" (bodymask (i) \"sain baina uu\")
end))

SSScript.External API:

(* [notify context message] notify about current context with message *)
val notify: context -> string -> unit

(* [queue context filepath] queue specified path for full scan (all checks from all plugins will be applied) *)
val queue: context -> string -> unit

(* shortcut to notify + condition *)
val rule: context -> string -> bool -> unit

(* shortcut to queue + condition *)
val queueif: context -> string -> bool -> unit

(* returns file size of context *)
val filesize: context -> Int64.t

(* returns true if specified file exists. Path is relative to context *)
val exists: context -> string -> bool

(* [with_file context file fn] call function fn with context of file 'file' *)
val with_file: context -> string -> (context -> unit) -> unit

CREATING NEW RULES USING LOW LEVEL API:

Function 'check' accepts argument of type 'context' as it's argument:

type fileinfo = {
  stat : Unix.LargeFile.stats;
  filename : string;
  body : string option lazy_t;
}

type context = {
  mutable current : fileinfo;
  mutable stack : fileinfo list;
}

" ^ ArgPipeFormat.usageInOut ^ "
KNOWN OPTIONS
" 


let main () =
  let open Lwt in
  SSConfig.get !configReaders
  >>= fun scripts ->
  let lst = List.map (fun (domain, script) -> (SSConfig_sig.string_of_domain domain), script) scripts in
  Lwt_list.iter_s (fun (domain, script) ->
      Lwt.catch
        (fun () ->
           SSExternalLoad.load_remote ~temp_dir:!temp_dir ~farm:!compile_farm script
        )
        (function
          | SSExternalLoad.CompileError (msg, fatal) ->
            Printf.eprintf "Failed to load script '%s':\n\n%s\n" domain msg;
            exit 1
          | exn ->
            Lwt.fail exn
        )
    ) lst
  >>= fun () ->

  let module IN_FORMAT = (val !ArgPipeFormat.input_format) in
  let module OUT_FORMAT = (val !ArgPipeFormat.output_format) in
  let module P = PipeLwt.Make (PipeFmtMain.Type) (IN_FORMAT) (OUT_FORMAT) in
  let pipe = P.init Lwt_io.stdin Lwt_io.stdout in
  P.iter_input
    (fun line ->
       match line with
       | Pipe.Record record ->
         let msgs = ref [] in
         let rec check rec_deepness file =
           if rec_deepness > 10 then
             ()
           else (
             try
               let context = SSScript.context file.PipeFmtMain.Type.file in
               let has_output = ref false in
               let register_output stack alert =
                 let open SSScript in
                 msgs := PipeFmtMain.Type.{ file with alert; file = stack.current.filename } :: !msgs;
                 has_output := true;
               in
               let queuefile_cb context_stack filename =
                 let open SSScript in
                 let dir = Filename.dirname context_stack.current.filename in
                 let file = Printf.sprintf "%s%s%s" dir Filename.dir_sep filename in
                 let file = PipeFmtMain.Type.{
                     file;
                     alert = "";
                     remote_ip = record.remote_ip;
                     username = record.username;
                     tail = []
                   }
                 in
                 check (rec_deepness+1) file
               in
               
               SSScript.External.set_notify register_output;
               SSScript.External.set_queuefile queuefile_cb;
               SSExternals.run file.PipeFmtMain.Type.file;

               register_output context file.PipeFmtMain.Type.alert
             with
             | _ ->
               (* file not found *)
               msgs := record :: !msgs
           )
         in
         check 0 record;
         let output file =
           P.output pipe (Pipe.Record file)
         in
         Lwt_list.iter_s output (List.rev !msgs)
      | _ ->
        P.output pipe line
    ) pipe

let () =
  Arg.parse args (fun _ -> ()) usage;
  Lwt_main.run (main ())
