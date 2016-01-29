
module XA = XMLAccess.F (Xml)

let fatal s =
  Printf.eprintf "FATAL: %s\n" s;
  exit 1

let get_attribute node name =
  try
    Xml.attrib node name
  with
  | Xml.No_attribute _ ->
    fatal (Printf.sprintf "Attribute %s not found" name)

let to_sl t =
  let open XA in
  t $$? "signature"
  $$> (fun s ->
      let regexp = content s in
      let fmt = get_attribute s "format" in
      if fmt <> "re" then
        fatal (Printf.sprintf "Unsupported Manul format: %s" fmt);
      let title = get_attribute s "title" in
      let open SexpLoc in
      list [
        atom "if"; list [atom "bodymask"; list []; atom regexp];
        list [atom "notify"; atom title]
      ]
    )

let () =
  let rules = Xml.parse_in stdin |> to_sl in
  let open SexpLoc in
  let sl = list [
      atom "if"; list [atom "filemask"; list [atom "i"]; atom "\\.php[3456789]?$"];
      list (atom "seq" :: rules)
    ]
  in
  SexpLoc.print sl
