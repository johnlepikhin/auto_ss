
let configReaders = ref [
  "arg";
]

let compile_farm = ref [
    "127.0.0.1:800"
  ]

let set_compile_farm =
  let rex = Pcre.regexp "," in
  fun s ->
    compile_farm := Pcre.split ~rex s

let args = ArgPipeFormat.argsInOut @ Arg.[
    "-r", String (fun s -> configReaders := s :: !configReaders), "Add config new reader";
    "-I", String (fun _ -> ()), "Inline check with specified condition";
    "-c", String set_compile_farm, "Location of compile farm. For example: 127.0.0.1:80,farm.localnet:8080 (default 127.0.0.1:800)"
]

let usage = "
TYPICAL USAGE

find /var/www -type f | ss -I 'rule \"bodymask () \"substring1\" \"substring2\")'

You can add more config sources with option -r. Option can be repeated. Possible values:

 * 'file:/path/to/file1:/path/to/file2:...'

will sequentially read configs from specified files

 * 'dir:/path/to/dir1:/path/to/file2:...'

will sequentially read configs from files in specified directories.


LANGUAGE DESRIPTION

This is Ocaml with limited functions access and some DSL:

 * rule \"Always matched rule\" true;;

Rule witch will match on all files

 * rule \"PHP files\" (filemask () \"php[345]?$\");;

Rule matching files which names matching regexp php[345]?$

 * rule \"PHP files\" (filemask (i) \"php[345]?$\");;

The same but case-less

Possible regexp flags are: i = caseless, m = multiline, s = dotall, x = extended, u = utf-8
Flags are space-separated: filemask (i m s) \".*\"

 * rule \"PHP files\" (bodymask () \"<html>\");;

Rule matching files with bodies matching regexp \"<html>\"

 * rule \"Russian substring\" (rusbodymask \"строка\")

Rule matching files with bodies matching regexp \"строка\" in charsets UTF-8,
koi8-r, cp1251, cp866

OTHER targets

 * queuefile \"other/file/to/check.php\" condition

Adds into check queue specified if condition is true.

OTHER conditions

 * exists \"file.php\"

Checks if file \"file.php\" exists (path is relative to current file)

 * filesize > 1000L

Checks if current file size > 1000 bytes


Boolean logic:

 * rule \"Logic OR\" (true || false)
 * rule \"Logic AND\" (true && false)
 * rule \"Logic NOT\" (not false)
 * rule \"Extended logic\" (not false && true || (true or false))

Define constant:

let is_php = filemask (i) \"php[345]?$\";;

Use constant:

rule \"Is PHP script\" is_php;;
" ^ ArgPipeFormat.usageInOut ^ "
KNOWN OPTIONS
" 


let main () =
  let open Lwt in
  SSConfig.get !configReaders
  >>= fun scripts ->
  let lst = List.map (fun (domain, script) -> (SSConfig_sig.string_of_domain domain), script) scripts in
  Lwt_list.iter_s (fun (domain, script) ->
      try%lwt
        SSExternalLoad.load_remote !compile_farm script
      with
      | SSExternalLoad.CompileError (msg, fatal) ->
        Printf.eprintf "Failed to load script '%s':\n\n%s\n" domain msg;
        exit 1
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
