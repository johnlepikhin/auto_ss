
open Lwt

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

module Context =
struct
  type t = {
    filename : string;
    body : string Lwt.t lazy_t;
    filemask_result : Superex.GroupsSet.t lazy_t;
    bodymask_result : Superex.GroupsSet.t Lwt.t lazy_t;
  }

  let readfile filename =
    Lazy.from_fun (fun () ->
        Lwt_io.(with_file ~mode:input filename (read ~count:(1024*1024)))
      )

  let init context_info =
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
            let open Lwt in
            Lazy.force body
            >>= fun body ->
            let r = ASTRegexp.apply context_info.ContextInfo.superex_body body in
            return r
          );
    }
end

include AST.Make
    (struct type t = ASTRegexp.regexp end)
    (ContextInfo)

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

and of_ast_filemask context_info lst =
  let (context_info, lst) =
    List.fold_left
      (fun (context_info, rlst) rex ->
         let open ContextInfo in
         let (superex_filename, rex) =
           ASTRegexp.register ~rex context_info.superex_filename
         in
         let context_info = { context_info with superex_filename } in
         context_info, (rex :: rlst)
      ) (context_info, []) lst
  in
  (context_info, Filemask lst)

and of_ast_bodymask context_info lst =
  let (context_info, lst) =
    List.fold_left
      (fun (context_info, rlst) rex ->
         let open ContextInfo in
         let (superex_body, rex) =
           ASTRegexp.register ~rex context_info.superex_body
         in
         let context_info = { context_info with superex_body } in
         context_info, (rex :: rlst)
      ) (context_info, []) lst
  in
  (context_info, Filemask lst)

and of_ast_bool context_info = function
  | AST.And l -> of_ast_and context_info l
  | AST.Or l -> of_ast_or context_info l
  | AST.Not expr ->  of_ast_not context_info expr
  | AST.True -> context_info, True
  | AST.False -> context_info, False
  | AST.Filemask l -> of_ast_filemask context_info l
  | AST.Bodymask l -> of_ast_bodymask context_info l

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
  in
  let context_info = ContextInfo.init "" in
  let context_info, t = loop context_info t in
  let context_info = ContextInfo.compile context_info in
  context_info, t

(********************* Apply **************************)

let apply_notify context_info context s =
  Printf.printf "context_info.filename=%s  context.filename=%s  : %s\n"
    context_info.ContextInfo.filename
    context.Context.filename
    s;
  return ()

let rec apply_and context_info context lst =
  let rec loop = function
    | [] ->
      return true
    | hd :: tl ->
      apply_bool context_info context hd
      >>= fun r ->
      if r then
        loop tl
      else
        return false
  in
  loop lst

and apply_or context_info context lst =
  let rec loop = function
    | [] ->
      return false
    | hd :: tl ->
      apply_bool context_info context hd
      >>= fun r ->
      if r then
        return true
      else
        loop tl
  in
  loop lst

and apply_not context_info context expr =
  apply_bool context_info context expr
  >>= fun r -> return (not r)

and apply_filemask context_info context lst =
  let rec loop = function
    | [] ->
      return false
    | regexp :: tl ->
      let result = Lazy.force context.Context.filemask_result in
      if ASTRegexp.matches regexp result then
        return true
      else
        loop tl
  in
  loop lst

and apply_bodymask context_info context lst =
  let rec loop = function
    | [] ->
      return false
    | regexp :: tl ->
      Lazy.force context.Context.bodymask_result
      >>= fun result ->
      if ASTRegexp.matches regexp result then
        return true
      else
        loop tl
  in
  loop lst

and apply_bool context_info context = function
  | And lst -> apply_and context_info context lst
  | Or lst -> apply_or context_info context lst
  | Not expr -> apply_not context_info context expr
  | True -> return true
  | False -> return false
  | Filemask lst -> apply_filemask context_info context lst
  | Bodymask lst -> apply_bodymask context_info context lst

let apply t context_info filename =
  let rec loop context_info context = function
    | Notify s -> apply_notify context_info context s
    | If (expr, app) ->
      apply_bool context_info context expr
      >>= fun r ->
      if r then
        loop context_info context app
      else
        return ()
    | SetContext (new_context_info, app) ->
      let new_context = Context.init new_context_info in
      loop new_context_info new_context app
  in
  let context_info = ContextInfo.updateFile context_info filename in
  let context = Context.init context_info in
  loop context_info context t
