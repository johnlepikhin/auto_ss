
let args = ArgPipeFormat.argsOut

let usage = ArgPipeFormat.usageOut ^ "\nCOMMAND LINE ARGUMENTS:\n"

module Parser =
struct
  let get_siteroot username =
    match Os.v with
    | Os.UNIX -> Filename.concat ("/home/virtwww/w_" ^ username) "http"
    | Os.Windows -> Filename.concat "d:\\web\\1Gb.ru\\hosted" username

  let index_files = [
    "/index.htm";
    "/index.html";
    "/index.php";
    "/index.php3";
    "/index.php4";
    "/index.php5";
  ]
  
  let parse =
    let open Pcre in
    (* 82.145.210.159 - - [16/Oct/2011:01:05:16 +0400] "GET /favicon.ico HTTP/1.1" 200 *)
    let rex = regexp "^([^ ]+) [^ ]+ [^ ]+ \\[[^\\]]+\\] \"[^ ]+ ([^ \\?]+)[^\"]*\" (\\d+)" in
    fun line values ->
      try
        let subs = exec ~rex line in
        let remote_ip = get_substring subs 1 in
        let username = List.assoc "username" values in
        let uri = get_substring subs 2 in
        let full_fpath = get_siteroot username in
        let uris =
          if uri = "/" then
            index_files
          else
            [uri]
        in
        List.map (fun uri ->
            let file = full_fpath ^ uri in
            PipeFmtMain.Type.{
                file;
                alert = "";
                remote_ip;
                username;
                tail = [];
            }
          ) uris
      with
      | _ -> []
end


let () =
  Arg.parse args (fun _ -> ()) usage;
  let module IN_FORMAT = (val !ArgPipeFormat.input_format) in
  let module OUT_FORMAT = (val !ArgPipeFormat.output_format) in
  let module Processor = LogParse.Make (PipeUnix.IO) (IN_FORMAT) (OUT_FORMAT) (Parser) in
  Processor.process ()
