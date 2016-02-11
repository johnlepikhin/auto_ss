
module CondMap = Map.Make (struct type t = int let compare = compare end)

type body_regexp = {
  id : int;
  fn : string;
  flags : Pcre.cflag list;
  regexps : string list;
  rex : Pcre.regexp;
}

type prepared = {
  state : ScriptInterp.state;
  body_regexp_map : body_regexp CondMap.t;
  filename_regexp_map : body_regexp CondMap.t;
}

type fileinfo = {
  filename : string;
  body : string option lazy_t;
  filename_regexp_cache : (int, bool) Hashtbl.t;
  body_regexp_cache : (int, bool) Hashtbl.t;
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
    filename;
    body = readfile filename;
    filename_regexp_cache = Hashtbl.create 101;
    body_regexp_cache = Hashtbl.create 101;
  }

let generator =
  let id = ref 0 in
  fun () ->
    incr id;
    !id

let prepare ?debug sources =
  let empty name () =
    failwith (Printf.sprintf "SSScript %s() ccall implementation required!" name)
  in
  let external_fns = ScriptInterp.[
      ext_fn "rule" ["string"; "bool"] "unit" (Obj.repr (empty "rule"));
      ext_fn "match_bodymask" ["int"] "bool" (Obj.repr (empty "match_bodymask"));
      ext_fn "match_filemask" ["int"] "bool" (Obj.repr (empty "match_filemask"));
    ]
  in

  let (world, initial_env) =
    ScriptExternal.world_of_externals external_fns
    |> ScriptHelpers.addBoolean
    |> ScriptHelpers.addComparsions
    |> ScriptHelpers.addIntegers
    |> ScriptHelpers.addCompositions
  in
  let body_regexp_map = ref CondMap.empty in
  let body_regexp_revmap = Hashtbl.create 101 in
  let filename_regexp_map = ref CondMap.empty in
  let filename_regexp_revmap = Hashtbl.create 101 in
  let register_regexp ~fname ~flags regexps =
    match fname with
    | "bodymask" -> (
      try
        Hashtbl.find body_regexp_revmap (flags, regexps)
      with
      | _ ->
        let id = generator () in
        let rex = Pcre.regexp_or ~flags regexps in
        body_regexp_map := CondMap.add id { id; fn = fname; flags; regexps; rex } !body_regexp_map;
        Hashtbl.add body_regexp_revmap (flags, regexps) id;
        id
      )
    | "filemask" -> (
      try
        Hashtbl.find filename_regexp_revmap (flags, regexps)
      with
      | _ ->
        let id = generator () in
        let rex = Pcre.regexp_or ~flags regexps in
        filename_regexp_map := CondMap.add id { id; fn = fname; flags; regexps; rex } !filename_regexp_map;
        Hashtbl.add filename_regexp_revmap (flags, regexps) id;
        id
      )
    | rextype ->
      failwith (Printf.sprintf "Undefined regexp type: %s" rextype)
  in
  let mapper = AstMapper.my_mapper register_regexp in

  let sources = List.map
      (fun (fileName, script) -> ScriptParse.init ~fileName script)
      sources
  in
  let parsed = ScriptParse.parse ?debug ~initial_env ~mapper ~moduleName:"Main" sources in
  let compiled = ScriptParse.compile ?debug parsed in
  let state = ScriptInterp.init ~world ~stackSize:16000 compiled.ScriptParse.instr in
  {
    state;
    body_regexp_map = !body_regexp_map;
    filename_regexp_map = !filename_regexp_map;
  }

let match_bodymask ?(debug=false) prepared fileinfo id =
  try
    let r = Hashtbl.find fileinfo.body_regexp_cache id in
    if debug then Printf.printf "match_bodymask (id=%i): cached value found\n" id;
    r
  with
  | _ ->
    if debug then Printf.printf "match_bodymask (id=%i): cached value not found\n" id;
    try
      let rex = CondMap.find id prepared.body_regexp_map in
      if debug then Printf.printf "match_bodymask (id=%i): regexp found\n" id;
      let body = Lazy.force fileinfo.body in
      match body with
      | None ->
        if debug then Printf.printf "match_bodymask (id=%i): no body\n" id;
        false
      | Some body ->
        let r = Pcre.pmatch ~rex:rex.rex body in
        if debug then Printf.printf "match_bodymask (id=%i): match result %B\n" id r;
        Hashtbl.add fileinfo.body_regexp_cache id r;
        r
    with
    | _ -> false

let match_filemask ?(debug=false) prepared fileinfo id =
  try
    Hashtbl.find fileinfo.filename_regexp_cache id
  with
  | _ ->
    try
      let rex = CondMap.find id prepared.filename_regexp_map in
      let r = Pcre.pmatch ~rex:rex.rex fileinfo.filename in
      Hashtbl.add fileinfo.filename_regexp_cache id r;
      r
    with
    | _ -> false

let run ?(debug=false) ~notify_cb ~script fileinfo =
  let open ScriptInterp in
  let rule name = function
    | false -> ()
    | true ->
      notify_cb fileinfo name
  in
  
  let notify_ccall =
    ext_fn "notify" ["string"] "unit" (Obj.repr (notify_cb fileinfo))
  in
  let match_bodymask_ccall =
    ext_fn "match_bodymask" ["int"] "bool" (Obj.repr (match_bodymask ~debug script fileinfo))
  in
  let match_filemask_ccall =
    ext_fn "match_filemask" ["int"] "bool" (Obj.repr (match_filemask ~debug script fileinfo))
  in
  let rule_ccall =
    ext_fn "rule" ["string"; "bool"] "unit" (Obj.repr rule)
  in
  let state = makeReadyCopy script.state in
  let world =
    add_external match_bodymask_ccall state.world
    |> add_external match_filemask_ccall
    |> add_external notify_ccall
    |> add_external rule_ccall
  in
  let state = { state with world } in
  if debug then
    interp_debug state
  else
    interp state
