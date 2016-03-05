
module Make (T : Pipe.TYPE) =
struct
  type record = T.record
  type meta = T.meta
  
  let of_string s =
    let open String in
    let l = length s in
    if l > 1 && s.[0] = '/' && s.[1] = '/' then
      let line = sub s 2 (l-2) in
      let meta = T.meta_of_line line in
      Pipe.Meta meta
    else
      let record = T.record_of_fields [s] in
      Pipe.Record record
  
  let to_string = function
    | Pipe.Record r ->
      T.fields_of_record r |> String.concat ""
    | Pipe.Meta r ->
      T.line_of_meta r
  
  let record_separator = '\000'

  let name = "findprint0"
end
