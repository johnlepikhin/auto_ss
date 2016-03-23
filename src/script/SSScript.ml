
let version = 21

type body_regexp = {
  id : int;
  fn : string;
  flags : Pcre.cflag list;
  regexps : string list;
  rex : Pcre.regexp;
}

type fileinfo = {
  stat : Unix.LargeFile.stats;
  filename : string;
  body : string option lazy_t;
}

type context_stack = {
  current : fileinfo;
  stack : fileinfo list;
}

let readfile filename =
  let aux () =
    let open Unix.LargeFile in
    try
      let stat = stat filename in
      let readsize = min 409600L stat.st_size |> Int64.to_int in
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
  in
  Lazy.from_fun aux

let fileinfo filename =
  {
    stat = Unix.LargeFile.stat filename;
    filename;
    body = readfile filename;
  }

let context filename =
  {
    current = fileinfo filename;
    stack = [];
  }

module External =
struct

  let _notify_cb = ref (fun (context : context_stack) (msg : string) -> ())
  let _queuefile_cb = ref (fun (context : context_stack) (next_file : string) -> ())

  let set_notify cb = _notify_cb := cb
  let set_queuefile cb = _queuefile_cb := cb
  
  let notify s = (!_notify_cb) s
  let queue s = (!_queuefile_cb) s

  let match_bodymask rex context =
    let body = Lazy.force context.current.body in
    match body with
    | None ->
      false
    | Some body ->
      Pcre.pmatch ~rex body

  let match_filemask rex context =
    Pcre.pmatch ~rex context.current.filename

  let rule context msg condition =
    if condition then
      notify context msg

  let queueif context file condition =
    if condition then
      (!_queuefile_cb) context file

  let filesize context =
    context.current.stat.Unix.LargeFile.st_size

  let exists context fname =
    let dir = Filename.dirname context.current.filename in
    let file = Filename.concat dir fname in
    Sys.file_exists file

  let with_file context file fn =
    let current = fileinfo file in
    fn { current; stack = context.current :: context.stack }
end
