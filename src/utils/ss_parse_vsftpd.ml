

let output_format = ref (module PipeNullChar.Make : Pipe.PIPE_FORMAT)

let set_format v f =
  let m =
    match f with
    | "shellescape" -> (module PipeShell.Make : Pipe.PIPE_FORMAT)
    | "nullchar" -> (module PipeNullChar.Make : Pipe.PIPE_FORMAT)
    | "human" -> (module PipeHuman.Make : Pipe.PIPE_FORMAT)
    | _ ->
      Printf.eprintf "Unknown pipe format: %s\n" f;
      exit 1
  in
  v := m

let args = Arg.[
    "-of", String (set_format output_format), "Pipe format for STDOUT";
]

let usage = ""

module Parser =
struct
  let get_siteroot username =
    match Os.v with
    | Os.UNIX -> Filename.concat (Filename.concat "/home/virtwww" username) "http"
    | Os.Windows -> Filename.concat "d:\\web\\1Gb.ru\\hosted" username

  let parse =
    let open Pcre in
    let rex = regexp "^[^ ]+ [^ ]+ \\d+ \\d+:\\d+:\\d+ \\d+ \\[pid \\d+\\] \\[w_([^\\]]+)\\] OK UPLOAD: Client \"([^\"]+)\", \"(.*)\", \\d+ bytes" in
    fun line values ->
      try
        let subs = Pcre.exec ~rex line in
        let username = Pcre.get_substring subs 1 in
        let remote_ip = Pcre.get_substring subs 2 in
        let fpath = Pcre.get_substring subs 3 in
        let siteroot = get_siteroot username in
        let file = Filename.concat siteroot fpath in
        Some PipeFmtMain.Type.{
            file;
            alert = "";
            remote_ip;
            username;
            tail = [];
          }
      with
      | _ -> None
end


let () =
  Arg.parse args (fun _ -> ()) usage;
  let module OUT_FORMAT = (val !output_format) in
  let module Processor = LogParse.Make (PipeUnix.IO) (PipeHuman.Make) (OUT_FORMAT) (Parser) in
  Processor.process ()
