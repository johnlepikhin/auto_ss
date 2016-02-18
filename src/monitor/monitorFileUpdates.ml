
type path_element =
  | Path of string
  | Regexp of Pcre.regexp

type path_mask = path_element list


let readdir path = function
  | Path e ->
    let path = Filename.concat path e in
    if Sys.file_exists path then
      [path]
    else
      []
  | Regexp rex ->
    try
      Sys.readdir path
      |> Array.to_list
      |> List.filter (fun n -> Filename.concat path n |> Pcre.pmatch ~rex)
      |> List.map (Filename.concat path)
    with
    | _ -> []

let getfiles mask =
  let rec iter curpath = function
    | [] ->
      failwith "Path cannot be empty"
    | [el] ->
      readdir curpath el
    | hd :: tl ->
      readdir curpath hd
      |> List.map (fun path -> iter path tl)
      |> List.concat
  in
  iter "" mask

module Collection = Map.Make (String)

let statfiles lst =
  List.fold_left (fun collection path ->
      let st = Unix.LargeFile.stat path in
      Collection.add path (st, 0L, st.Unix.LargeFile.st_size) collection
    ) Collection.empty lst

let getdiff prev next =
  let open Unix.LargeFile in
  Collection.fold (fun path ((next_st, _, _) as next) rcollection ->
      try
        (* update file *)
        let (prev_st, prev_begin, prev_end) = Collection.find path prev in
        if prev_st.st_size > next_st.st_size then
          (* file is zeroed? *)
          Collection.add path next rcollection
        else
        if prev_st.st_size < next_st.st_size then
          (* file has new lines? *)
          Collection.add path (next_st, prev_end, next_st.st_size) rcollection
        else
          (* file is not changed *)
          rcollection
      with
      | _ ->
        (* this is new file *)
        Collection.add path next rcollection
    ) next Collection.empty
