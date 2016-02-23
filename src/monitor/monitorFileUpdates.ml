
type path_element =
  | Path of string
  | Regexp of Pcre.regexp * (string array) * string

type path_mask = path_element list

let groups_of_regexp =
  let open Pcre in
  let rex = regexp "%([^%]+)%" in
  let repl_rex = regexp "%[^%]+%" in
  fun s ->
    try
      let subs = exec_all ~rex s in
      let groups = Array.map (fun sub -> get_substring sub 1) subs in
      let s' = replace ~rex:repl_rex ~templ:"(.*?)" s in
      Regexp (regexp s', groups, s)
    with
    | _ ->
      Regexp (regexp s, [||], s)

type path = string

type info = {
  path : string;
  st : Unix.LargeFile.stats;
  pos_begin : int64;
  pos_end : int64;
  values : (string * string) list;
}

type state = (path, info) Hashtbl.t

let timeout_mtime = 90000.

let getfiles (mask : path_element list) =
  let h : state = Hashtbl.create 7919 in
  let now = Unix.time () in
  
  let result_of_regexp path values rex groups src =
    let rec map = function
      | name :: tl -> (
        try
          let subs = Pcre.exec ~rex name in
          let (values, _) =
            Array.fold_left (fun (values, pos) name -> (name, Pcre.get_substring subs pos) :: values, pos + 1) (values, 1) groups
          in
          let path = Filename.concat path name in
          let open Unix.LargeFile in
          let st = stat path in
          if st.st_mtime +. timeout_mtime > now then (
            Hashtbl.add h path {
              path;
              st;
              pos_begin = 0L;
              pos_end = st.Unix.LargeFile.st_size;
              values;
            };
            (path, values) :: map tl
          ) else
            map tl
        with
        | _ ->
          map tl
        )
      | [] -> []
    in
    try
      Sys.readdir path
      |> Array.to_list
      |> map
    with
    | _ -> []
  in
  
  let readdir path values = function
    | Path e -> (
        let path = Filename.concat path e in
        try
          let open Unix.LargeFile in
          let st = stat path in
          if st.st_mtime +. timeout_mtime > now then (
            Hashtbl.add h path {
              path;
              st;
              pos_begin = 0L;
              pos_end = st.st_size;
              values;
            };
            [path, values]
          ) else
            []
        with
        | _ -> []
      )
    | Regexp (rex, groups, src) ->
      result_of_regexp path values rex groups src
  in
  let rec iter curpath values = function
    | [] ->
      failwith "Path cannot be empty"
    | [el] ->
      readdir curpath values el
    | hd :: tl ->
      readdir curpath values hd
      |> List.map (fun (path, values) -> iter path values tl)
      |> List.concat
  in
  ignore (iter "" [] mask);
  h

let getdiff (prev : state) (next : state) =
  let open Unix.LargeFile in
  let r = ref [] in
  Hashtbl.iter (fun (path : path) nextinfo ->
      try
        (* update file *)
        let previnfo = Hashtbl.find prev path in
        if previnfo.st.st_size > nextinfo.st.st_size then
          (* file is zeroed? *)
          r := nextinfo :: !r
        else
        if previnfo.st.st_size < nextinfo.st.st_size then
          (* file has new lines? *)
          r := { nextinfo with
                 pos_begin = previnfo.pos_end;
                 pos_end = nextinfo.pos_end;
          } :: !r
        else
          (* file is not changed *)
          ()
      with
      | _ ->
        (* this is new file *)
        r := nextinfo :: !r
    ) next;
  !r
