
let configReaders = ref [
  "arg";
]

let args = Arg.[
    "-r", String (fun s -> configReaders := s :: !configReaders), "Add config new reader";
    "--script", String (fun _ -> ()), "Pass additional rule";
]

let usage = "
TYPICAL USAGE

find /var/www -type f | ss --script 'rule \"Some issue\" (bodymask () \"substring1\" \"substring2\")'

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

 * rule \"PHP files\" (bodymask () \"<html>\");;

Rule matching files with bodies matching regexp \"<html>\"

 * rule \"Russian substring\" (rusbodymask \"строка\")

Rule matching files with bodies matching regexp \"строка\" in charsets UTF-8,
koi8-r, cp1251, cp866

Boolean logic:

 * rule \"Logic OR\" (true || false)
 * rule \"Logic AND\" (true && false)
 * rule \"Logic NOT\" (not false)
 * rule \"Extended logic\" (not false && true || (true or false))

Define constant:

let is_php = filemask (i) \"php[345]?$\";;

Use constant:

rule \"Is PHP script\" is_php;;

KNOWN OPTIONS
"

module Pipeline = PipeLwt.Make (PipeShell)

(*
module Evaluator = ASTOptimized.MakeEvaluator (
  struct
    type t = UtilPipe.file

    let notify (context_info : ASTOptimized.ContextInfo.t) (context : t) alert =
      let open ASTOptimized in
      let file = UtilPipe.{ context with alert } in
      let line = UtilPipe.(to_pipe (File file)) in
      Pipeline.output Lwt_io.stdout line

  end)
*)

let main () =
  let open Lwt in
  SSConfig.get !configReaders
  >>= fun scripts ->

  if scripts = [] then (
    Printf.eprintf "No configs specified\n";
    exit 1
  );
    
  let script =
    List.map (fun (domain, script) -> (SSConfig_sig.string_of_domain domain), script) scripts
    |> SSScript.prepare
  in

  let open Pipe.Sig in
  Pipeline.iter_input
    (fun pipe ->
       let utilpipe = UtilPipe.of_pipe pipe in
       match utilpipe with
       | UtilPipe.File file ->
         let msgs = ref [] in
         let rec check rec_deepness file =
           if rec_deepness > 10 then
             ()
           else (
             let fileinfo = SSScript.fileinfo file.UtilPipe.file in
             let has_output = ref false in
             let register_output fileinfo alert =
               msgs := UtilPipe.{ file with alert } :: !msgs;
               has_output := true;
             in
             let queuefile_cb fileinfo filename =
               let dir = Filename.dirname fileinfo.SSScript.filename in
               let file = Printf.sprintf "%s%s%s" dir Filename.dir_sep filename in
               let file = UtilPipe.{ file; alert = ""; tail = [] } in
               check (rec_deepness+1) file
             in

             SSScript.run ~notify_cb:register_output ~queuefile_cb ~script fileinfo;

             if not (UtilPipe.file_is_empty file) || not !has_output then
               register_output fileinfo file.UtilPipe.alert
           )
         in
         check 0 file;
         let open UtilPipe in
         let output file =
           let line = (to_pipe (File file)) in
           Pipeline.output Lwt_io.stdout line
         in
         Lwt_list.iter_s output (List.rev !msgs)
      | _ ->
        Pipeline.output Lwt_io.stdout pipe
    ) Lwt_io.stdin

let main () =
  let error_cb msg =
    Printf.eprintf "%s\n" msg;
    exit 1
  in
  ScriptParse.wrapped ~error_cb main

let () =
  Arg.parse args (fun _ -> ()) usage;
  Lwt_main.run (main ())
