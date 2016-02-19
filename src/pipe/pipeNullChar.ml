
module Make (T : Pipe.TYPE) =
struct
  type record = T.record
  type meta = T.meta
  
  let unescape =
    let open Pcre in
    let rex1 = regexp "\\\\001" in
    let rexslash = regexp "\\\\\\\\" in
    fun s ->
      Pcre.replace ~rex:rex1 ~templ:"\001" s
      |> Pcre.replace ~rex:rexslash ~templ:"\\"

  let escape =
    let open Pcre in
    let rex1 = regexp "\\001" in
    let rexslash = regexp "\\\\" in
    fun s ->
      Pcre.replace ~rex:rexslash ~templ:"\\\\" s
      |> Pcre.replace ~rex:rex1 ~templ:"\\001"

  let of_string =
    let open Pcre in
    let rex = regexp "\001" in
    fun s ->
      let open String in
      let l = length s in
      if l > 1 && s.[0] = '/' && s.[1] = '/' then
        let line = sub s 2 (l-2) |> unescape in
        let meta = T.meta_of_line line in
        Pipe.Meta meta
      else
        let fields = split ~rex ~max:10000 s |> List.map unescape in
        let record = T.record_of_fields fields in
        Pipe.Record record
  
  let to_string = function
    | Pipe.Record r ->
      T.fields_of_record r
      |> List.map escape
      |> String.concat "\001"
    | Pipe.Meta r ->
      T.line_of_meta r
      |> escape
  
  let record_separator = '\000'
end
