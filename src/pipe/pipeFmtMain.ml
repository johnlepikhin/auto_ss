
module Type =
struct
  type record = {
    file : string;
    alert : string;
    remote_ip : string;
    tail : string list;
  }

  type meta = string

  let record_of_fields = function
    | file :: alert :: remote_ip :: tail ->
      {
        file;
        alert;
        remote_ip;
        tail;
      }
    | file :: alert :: tail ->
      {
        file;
        alert;
        remote_ip = "";
        tail = [];
      }
    | [file] ->
      {
        file;
        alert = "";
        remote_ip = "";
        tail = [];
      }
    | _ ->
      { file = ""; alert = ""; remote_ip = ""; tail = [] }

  let meta_of_line s = s

  let fields_of_record r =
    r.file :: r.alert :: r.remote_ip :: r.tail

  let line_of_meta r =
    r

  let file_is_empty f =
    f.alert = "" && not (List.exists (( <> ) "") f.tail)
end

