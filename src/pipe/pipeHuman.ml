
open Pipe.Sig

let file_of_string =
  let open Pcre in
  let rex = regexp "\t" in
  fun s ->
    let fields = split ~rex ~max:10000 s |> List.map ShellEscape.unescape_string in
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
    Meta (sub s 2 (l-2) |> ShellEscape.unescape_string)
  else
    File (file_of_string s)

let escape =
  let open Pcre in
  let rexn = regexp "\n" in
  let rext = regexp "\t" in
  let rexs = regexp "\\\\" in
  fun s ->
    replace ~rex:rexs ~templ:"\\\\" s
    |> replace ~rex:rext ~templ:"\\t"
    |> replace ~rex:rexn ~templ:"\\n"

let file_to_string r =
  List.map escape (r.file :: r.tail)
  |> String.concat "\t"

let to_string = function
  | File r -> file_to_string r
  | Meta r -> escape r

let record_separator = '\n'
