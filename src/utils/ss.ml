
let configReaders = ref [
  "slarg";
]

let args = Arg.[
    "-r", String (fun s -> configReaders := s :: !configReaders), "Add config new reader";
    "--slarg", String (fun _ -> ()), "Pass additional rule";
]

let usage = "
TYPICAL USAGE

find /var/www -type f | ss --slarg '(if (bodymask () \"substring\") (notify \"file matches\"))'

You can add more config sources with option -r. Option can be repeated. Possible values:

 * 'slfile:/path/to/file1:/path/to/file2:...'

will sequentially read configs from specified files

 * 'sldir:/path/to/dir1:/path/to/file2:...'

will sequentially read configs from files in specified directories.


LANGUAGE DESRIPTION

This is simplified variant of Lisp. All expressions are S-expressions.

 * (seq v1 v2 v3 ...)

Concatenate expressions into one sequence.

 * (if EXPR APP)

Apply APP if EXPR evaluates to true.

 * \"true\" or \"false\"

Returns boolean value respectively. Example: (if true ...)

 * (filemask FLAGS REGEXP)

Checks if REGEXP matches with current file name. FLAGS are pcre regexp flags: i s m ...

For example, case-insensitive multiline matching: (filemask (i m) \"[0-5] some regexp\")

 * (bodymask FLAGS REGEXP)

The same ase (filemask ...), but for matching file content (only first 400KB of file!)

 * (defmacro NAME ARGS BODY)

Define new macro with name NAME, arguments list ARGS and body BODY. Macro is visible only
in current (seq ...) block and all nested blocks. To make it simple, all configs are
nested sequentially into one global (seq ...). So macros defined on top of first config
will be visible in all next.

For example:

(seq
  (defmacro html () (filemask () \"html$\"))

  (if html (notify \"HTML file\")))

With arguments:

(seq
  (defmacro i-fmask (mask) (filemask (i) mask))

  (if (i-mask \"hTmL$\") (notify \"HTML file\")))

 * (iconv SRC DST STRING)

Convert STRING from SRC charset to DST.

Example: (iconv UTF-8 koi8-r \"some string\")

 * (set-context FILENAME BODY)

Switch to file FILENAME in BODY commands. This is usefull
when you want to check related file.

Example:

(if (filemask (i) \"index.html\")
  (set-context \"login.php\"
    (if (bodymask () \"$request = $_POST\")
      (notify \"Infected!\"))))


KNOWN OPTIONS
"

module Pipeline = PipeLwt.Make (PipeShell)

module Evaluator = ASTOptimized.MakeEvaluator (
  struct
    type t = UtilPipe.file

    let notify (context_info : ASTOptimized.ContextInfo.t) (context : t) alert =
      let open ASTOptimized in
      let file = UtilPipe.{ context with alert } in
      let line = UtilPipe.(to_pipe (File file)) in
      Pipeline.output Lwt_io.stdout line

  end)

let main () =
  let open Lwt in
  Config.get !configReaders
  >>= fun (context_info, optimized) ->

  let open Pipe.Sig in
  Pipeline.iter_input
    (fun pipe ->
       let utilpipe = UtilPipe.of_pipe pipe in
       match utilpipe with
       | UtilPipe.File file ->
         Evaluator.apply optimized context_info file.UtilPipe.file file
      | _ ->
        Pipeline.output Lwt_io.stdout pipe
    ) Lwt_io.stdin

let () =
  Arg.parse args (fun _ -> ()) usage;
  Lwt_main.run (main ())
