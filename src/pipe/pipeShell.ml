
module Make (T : Pipe.TYPE) =
struct
  type record = T.record
  type meta = T.meta
  
  let of_string =
    let open Pcre in
    let rex = regexp "\t" in
    fun s ->
      let open String in
      let l = length s in
      if l > 1 && s.[0] = '/' && s.[1] = '/' then
        let line = sub s 2 (l-2) |> ShellEscape.unescape_string in
        let meta = T.meta_of_line line in
        Pipe.Meta meta
      else
        let fields = split ~rex ~max:10000 s |> List.map ShellEscape.unescape_string in
        let record = T.record_of_fields fields in
        Pipe.Record record

  let to_string = function
    | Pipe.Record r ->
      T.fields_of_record r
      |> List.map ShellEscape.escape_string
      |> String.concat "\t"
    | Pipe.Meta r ->
      T.line_of_meta r
      |> ShellEscape.escape_string

  let record_separator = '\n'

  let name = "shellescape"
end
