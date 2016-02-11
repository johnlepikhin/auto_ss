
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

let list_of_manul t =
  let open XA in
  t $$? "signature"
  $$> (fun s ->
      let regexp = content s in
      let fmt = get_attribute s "format" in
      if fmt <> "re" then
        fatal (Printf.sprintf "Unsupported Manul format: %s" fmt);
      let title = get_attribute s "title" in
      title, regexp
    )

let hash_of_list list =
  let h = Hashtbl.create 101 in
  List.iter (fun (title, regexp) ->
      try
        let l = Hashtbl.find h title in
        l := regexp :: !l
      with
      | _ ->
        Hashtbl.add h title (ref [regexp])
    ) list;
  h

let split_by_size ~limit lst =
  let rec aux len r = function
    | [] -> [r]
    | hd :: tl ->
      let hdlen = String.length hd in
      if len + hdlen > limit then
        r :: aux hdlen [hd] tl
      else
        aux (len+hdlen) (hd :: r) tl
  in
  aux 0 [] lst

let rules_of_hash hash =
  let b = Buffer.create 100000 in
  Hashtbl.iter (fun title regexps ->
      split_by_size ~limit:10000 !regexps
      |> List.iter (fun regexps ->
          let regexps = List.map (Printf.sprintf "%S") regexps |> String.concat " " in
          let rule =
            Printf.sprintf
              "rule %S (filemask (i) \"\\\\.php[3456789]?$\" && bodymask () %s);;\n\n"
              title
              regexps
          in
          Buffer.add_string b rule
        )
    ) hash;
  Buffer.contents b

let () =
  Xml.parse_in stdin
  |> list_of_manul
  |> hash_of_list
  |> rules_of_hash
  |> print_endline
