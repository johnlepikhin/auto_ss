
module ContextInfo =
struct
  type t = {
    filename : string;
    superex_filename : ASTRegexp.smap;
    superex_body : ASTRegexp.smap;
  }

  let init filename = {
    filename;
    superex_filename = ASTRegexp.initial;
    superex_body = ASTRegexp.initial;
  }

  let updateFile t filename = { t with filename }

  let compile t = {
    t with
    superex_filename = ASTRegexp.compile t.superex_filename;
    superex_body = ASTRegexp.compile t.superex_body;
  }
end

module type CONTEXT_EXT =
sig
  type t

  val notify: ContextInfo.t -> t -> string -> unit Lwt.t
end

module MakeContext (E : CONTEXT_EXT) =
struct
  type ext = E.t

  type t = {
    filename : string;
    body : string option lazy_t;
    filemask_result : ASTRegexp.result lazy_t;
    bodymask_result : ASTRegexp.result lazy_t;
    ext : E.t;
  }

  let readfile filename =
    let aux () =
      let open Unix.LargeFile in
      try
        let stat = stat filename in
        let readsize = min 409600L stat.st_size |> Int64.to_int in
        let buf = Buffer.create readsize in
        let ch = open_in filename in
        try
          Buffer.add_channel buf ch readsize;
          close_in ch;
          Some (Buffer.contents buf)
        with
        | _ ->
          close_in ch;
          None
      with
      | _ -> None
    in
    Lazy.from_fun aux

  let init ext context_info =
    let filename = context_info.ContextInfo.filename in
    let body = readfile filename in
    {
      filename;
      body;
      filemask_result =
        Lazy.from_fun (fun () ->
            ASTRegexp.apply context_info.ContextInfo.superex_filename filename);
      bodymask_result =
        Lazy.from_fun (fun () ->
            let body = Lazy.force body in
            match body with
            | None -> ASTRegexp.ResM.empty;
            | Some body ->
              ASTRegexp.apply context_info.ContextInfo.superex_body body
          );
      ext;
    }

  let notify = E.notify
end


include AST.Make
    (struct type t = ASTRegexp.regexp end)
    (ContextInfo)

module Parser =
struct

  let rec of_ast_and context_info (lst : AST.t_bool list) =
    let (context_info, lst) =
      List.fold_left
        (fun (context_info, rlst) expr ->
           let (context_info, (expr : t_bool)) = of_ast_bool context_info expr in
           context_info, (expr :: rlst)
        ) (context_info, []) lst
    in
    (context_info, And lst)

  and of_ast_or context_info (lst : AST.t_bool list) =
    let (context_info, lst) =
      List.fold_left
        (fun (context_info, rlst) expr ->
           let (context_info, (expr : t_bool)) = of_ast_bool context_info expr in
           context_info, (expr :: rlst)
        ) (context_info, []) lst
    in
    (context_info, Or lst)

  and of_ast_not context_info expr =
    let (context_info, expr) = of_ast_bool context_info expr in
    (context_info, Not expr)

  and of_ast_filemask context_info flags lst =
    let (context_info, lst) =
      List.fold_left
        (fun (context_info, rlst) rex ->
           let open ContextInfo in
           let (superex_filename, rex) =
             ASTRegexp.register ~rex:(rex, flags) context_info.superex_filename
           in
           let context_info = { context_info with superex_filename } in
           context_info, (rex :: rlst)
        ) (context_info, []) lst
    in
    (context_info, Filemask (flags, lst))

  and of_ast_bodymask context_info flags lst =
    let (context_info, lst) =
      List.fold_left
        (fun (context_info, rlst) rex ->
           let open ContextInfo in
           let (superex_body, rex) =
             ASTRegexp.register ~rex:(rex, flags) context_info.superex_body
           in
           let context_info = { context_info with superex_body } in
           context_info, (rex :: rlst)
        ) (context_info, []) lst
    in
    (context_info, Bodymask (flags, lst))

  and of_ast_bool context_info = function
    | AST.And l -> of_ast_and context_info l
    | AST.Or l -> of_ast_or context_info l
    | AST.Not expr ->  of_ast_not context_info expr
    | AST.True -> context_info, True
    | AST.False -> context_info, False
    | AST.Filemask (flags, l) -> of_ast_filemask context_info flags l
    | AST.Bodymask (flags, l) -> of_ast_bodymask context_info flags l

  let of_ast t =
    let rec loop context_info (t : AST.t) : (ContextInfo.t * t) =
      match t with
      | AST.Notify s -> context_info, Notify s
      | AST.If (expr, app) ->
        let (context_info, expr) = of_ast_bool context_info expr in
        let (context_info, app) = loop context_info app in
        context_info, If (expr, app)
      | AST.SetContext (file, app) ->
        let internal_context_info = ContextInfo.init file in
        let (internal_context_info, app) = loop internal_context_info app in
        let internal_context_info = ContextInfo.compile internal_context_info in
        context_info, SetContext (internal_context_info, app)
      | AST.Seq lst ->
        let rec aux context_info rlst = function
          | [] -> context_info, Seq rlst
          | app :: tl ->
            let (context_info, app) = loop context_info app in
            aux context_info (app :: rlst) tl
        in
        aux context_info [] lst
    in
    let context_info = ContextInfo.init "" in
    let context_info, t = loop context_info t in
    let context_info = ContextInfo.compile context_info in
    context_info, t
end


module MakeEvaluator (C : CONTEXT_EXT) =
struct
  module C = MakeContext (C)
  
  let rec apply_and context_info context lst =
    let rec loop = function
      | [] ->
        true
      | hd :: tl ->
        let r = apply_bool context_info context hd in
        if r then
          loop tl
        else
          false
    in
    loop lst

  and apply_or context_info context lst =
    let rec loop = function
      | [] ->
        false
      | hd :: tl ->
        let r = apply_bool context_info context hd in
        if r then
          true
        else
          loop tl
    in
    loop lst

  and apply_not context_info context expr =
    let r = apply_bool context_info context expr in
    not r

  and apply_filemask context_info context flags lst =
    let rec loop = function
      | [] ->
        false
      | regexp :: tl ->
        let result = Lazy.force context.C.filemask_result in
        if ASTRegexp.matches regexp result then
          true
        else
          loop tl
    in
    loop lst

  and apply_bodymask context_info context flags lst =
    let rec loop = function
      | [] ->
        false
      | regexp :: tl ->
        let result = Lazy.force context.C.bodymask_result in
        if ASTRegexp.matches regexp result then
          true
        else
          loop tl
    in
    loop lst

  and apply_bool context_info context = function
    | And lst -> apply_and context_info context lst
    | Or lst -> apply_or context_info context lst
    | Not expr -> apply_not context_info context expr
    | True -> true
    | False -> false
    | Filemask (flags, lst) -> apply_filemask context_info context flags lst
    | Bodymask (flags, lst) -> apply_bodymask context_info context flags lst

  let apply t context_info filename ext =
    let rec loop context_info context = function
      | Notify s -> C.notify context_info context.C.ext s
      | If (expr, app) ->
        let r = apply_bool context_info context expr in
        if r then
          loop context_info context app
        else
          Lwt.return ()
      | SetContext (new_context_info, app) ->
        let new_context = C.init ext new_context_info in
        loop new_context_info new_context app
      | Seq lst ->
        Lwt_list.iter_s (loop context_info context) lst
    in
    let context_info = ContextInfo.updateFile context_info filename in
    let context = C.init ext context_info in
    loop context_info context t
end

module Sample = MakeEvaluator (
struct
  type t = string
  
  let notify context_info context s =
    Printf.printf "context_info.filename=%s  context.filename=%s  : %s\n"
      context_info.ContextInfo.filename
      context
      s;
    Lwt.return ()
end)
