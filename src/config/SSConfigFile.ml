
module M :
sig
  include SSConfig_sig.CONFIGREADER
            
  val _get: string -> string list -> (SSConfig_sig.domain * string) list Lwt.t
end
=
struct
  let identifier = "slfile"

  let filelimit = 40960000L

  let readfile filename =
    let open Unix.LargeFile in
    try
      let stat = stat filename in
      let readsize = min filelimit stat.st_size |> Int64.to_int in
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

  let _get identifier params =
    if params = [] then
      raise (failwith (Printf.sprintf "Config reader '%s' requires at least one argument" identifier))
    else
      List.map (fun fname -> fname, readfile fname) params
      |> List.map (fun (fname, content) ->
          match content with
          | None -> raise (failwith (Printf.sprintf "Failed to read slfile: %s" fname))
          | Some content -> SSConfig_sig.File fname, content
        )
      |> Lwt.return
  
  let get params = _get identifier params
end

include M
