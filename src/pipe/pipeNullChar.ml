
open Pipe.Sig

let unescape =
  let open Pcre in
  let rex1 = regexp "\\001" in
  let rexslash = regexp "\\\\" in
  fun s ->
    Pcre.replace ~rex:rex1 ~templ:"\001" s
    |> Pcre.replace ~rex:rexslash ~templ:"\\"

let escape =
  let open Pcre in
  let rex1 = regexp "\001" in
  let rexslash = regexp "\\" in
  fun s ->
    Pcre.replace ~rex:rexslash ~templ:"\\\\" s
    |> Pcre.replace ~rex:rex1 ~templ:"\\001"

let file_of_string =
  let open Pcre in
  let rex = regexp "\001" in
  fun s ->
    let fields = split ~rex ~max:10000 s |> List.map unescape in
    match fields with
    | file :: tail ->
      {
        file;
        tail;
      }
    | _ -> { file = ""; tail = [] }

let of_string s =
  let open String in
  let l = length s in
  if l > 1 && s.[0] = '/' && s.[1] = '/' then
    Meta (sub s 2 (l-2) |> unescape)
  else
    File (file_of_string s)

let file_to_string r =
  List.map escape (r.file :: r.tail)
  |> String.concat "\001"

let to_string = function
  | File r -> file_to_string r
  | Meta r -> escape r

let record_separator = '\000'
