
module Type =
struct
  type record = {
    file : string;
    begin_pos : int64;
    end_pos : int64;
    values : (string * string) list;
  }

  type meta = string

  let value_of_string =
    let rex = Pcre.regexp "=" in
    fun s ->
      match Pcre.split ~rex s with
      | k :: tl ->
        k, String.concat "=" tl
      | _ ->
        failwith "Invalid value in PipeFmtLog input"

  let string_of_value (k, v) =
    Printf.sprintf "%s=%s" k v
  
  let record_of_fields = function
    | file :: begin_pos :: end_pos :: values -> (
      try
        {
          file;
          begin_pos = Int64.of_string begin_pos;
          end_pos = Int64.of_string end_pos;
          values = List.map value_of_string values;
        }
      with
      | _ ->
        failwith "Invalid PipeFmtLog input"
      )
    | _ ->
      failwith "Invalid PipeFmtLog input"

  let meta_of_line s = s

  let fields_of_record r =
    r.file :: Int64.to_string r.begin_pos :: Int64.to_string r.end_pos :: (List.map string_of_value r.values)

  let line_of_meta r =
    r
end

