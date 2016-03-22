
let version = 20

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
  mutable current : fileinfo;
  mutable stack : fileinfo list;
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
  let context_stack = ref (context "/")

  let _notify_cb = ref (fun (context : context_stack) (msg : string) -> ())
  let _queuefile_cb = ref (fun (context : context_stack) (next_file : string) -> ())

  let set_notify cb = _notify_cb := cb
  let set_queuefile cb = _queuefile_cb := cb
  let set_context c = context_stack := c
  
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

  let rule msg result =
    if result then
      notify !context_stack msg
end
