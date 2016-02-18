
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
      regexp s', groups, s
    with
    | _ ->
      regexp s, [||], s

let result_of_regexp path values rex groups src =
  let rec map = function
    | [] -> []
    | hd :: tl ->
      try
        let subs = Pcre.exec ~rex hd in
        let (values, _) =
          Array.fold_left (fun (values, pos) name -> (name, Pcre.get_substring subs pos) :: values, pos + 1) (values, 1) groups
        in
        ((Filename.concat path hd), values) :: map tl
      with
      | _ ->
        map tl
  in
  try
    Sys.readdir path
    |> Array.to_list
    |> map
  with
  | _ -> []

let readdir path values = function
  | Path e ->
    let path = Filename.concat path e in
    if Sys.file_exists path then
      [path, values]
    else
      []
  | Regexp (rex, groups, src) ->
    result_of_regexp path values rex groups src

let getfiles (mask : path_element list) =
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
  iter "" [] mask

module Collection = Map.Make (String)

let statfiles lst =
  List.fold_left (fun collection (path, values) ->
      let st = Unix.LargeFile.stat path in
      Collection.add path (st, 0L, st.Unix.LargeFile.st_size, values) collection
    ) Collection.empty lst

let getdiff prev next =
  let open Unix.LargeFile in
  Collection.fold (fun path ((next_st, _, _, values) as next) rcollection ->
      try
        (* update file *)
        let (prev_st, prev_begin, prev_end, values) = Collection.find path prev in
        if prev_st.st_size > next_st.st_size then
          (* file is zeroed? *)
          Collection.add path next rcollection
        else
        if prev_st.st_size < next_st.st_size then
          (* file has new lines? *)
          Collection.add path (next_st, prev_end, next_st.st_size, values) rcollection
        else
          (* file is not changed *)
          rcollection
      with
      | _ ->
        (* this is new file *)
        Collection.add path next rcollection
    ) next Collection.empty
