
let readers : (module SSConfig_sig.CONFIGREADER) list = [
  (module SSConfigFile);
  (module SSConfigDir);
  (module SSConfigArg);
]

let get =
  let splitRex = Pcre.regexp ":" in
  fun lst ->
    let open Lwt in
    let rec aux elt =
      let values = Pcre.split ~rex:splitRex elt in
      match values with
      | [] -> Lwt.fail (failwith "Config reader requires at least reader name (slfile, etc.)")
      | name :: args ->
        try
          let m = List.find (fun (module Reader : SSConfig_sig.CONFIGREADER) -> Reader.identifier = name) readers in
          let module Reader = (val m : SSConfig_sig.CONFIGREADER) in
          Reader.get args
        with
        | Not_found ->
          Lwt.fail (failwith (Printf.sprintf "Unknown config type: '%s'" name))
    in
    Lwt_list.map_s aux lst
    >>= fun lst ->
    return @@ List.concat lst
