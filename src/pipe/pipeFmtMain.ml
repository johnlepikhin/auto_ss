
module Type =
struct
  type record = {
    file : string;
    alert : string;
    tail : string list;
  }

  type meta = string

  let record_of_fields = function
    | file :: alert :: tail ->
      {
        file;
        alert;
        tail;
      }
    | [file] ->
      {
        file;
        alert = "";
        tail = [];
      }
    | _ ->
      { file = ""; alert = ""; tail = [] }

  let meta_of_line s = s

  let fields_of_record r =
    r.file :: r.alert :: r.tail

  let line_of_meta r =
    r

  let file_is_empty f =
    f.alert = "" && not (List.exists (( <> ) "") f.tail)
end

