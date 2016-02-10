
module CondMap = Map.Make (struct type t = int let compare = compare end)

module type EXTERNAL =
sig
  val notify: string -> unit
end

module Make (E : EXTERNAL) =
struct

  type condition = {
    id : int;
    fn : string;
    flags : Pcre.cflag list;
    regexps : string list;
  }
  
  type prepared = {
    state : ScriptInterp.state;
    condmap : condition CondMap.t;
  }

  let rule name = function
    | false -> ()
    | true -> E.notify name
  
  let prepare script =
    let external_fns = ScriptInterp.[
        ext_fn "rule" ["string"; "bool"] "unit" (Obj.repr rule);
        ext_fn "match_bodymask" ["int"] "bool" (Obj.repr (fun _ -> false));
      ]
    in

    let (world, env) =
      ScriptExternal.world_of_externals external_fns
      |> ScriptHelpers.addBoolean
      |> ScriptHelpers.addComparsions
      |> ScriptHelpers.addIntegers
      |> ScriptHelpers.addCompositions
    in
    let condmap = ref CondMap.empty in
    let register_regexp ~id ~fname ~flags regexps =
      condmap := CondMap.add id { id; fn = fname; flags; regexps } !condmap
    in
    let mapper = AstMapper.my_mapper register_regexp in
    
    let source = ScriptParse.init ~env ~fileName:"test" ~moduleName:"Test" script in
    let parsed = ScriptParse.parse ~mapper source in
    let compiled = ScriptParse.compile parsed in
    let state = ScriptInterp.init ~world ~stackSize:16000 compiled.ScriptParse.instr in
    {
      state;
      condmap = !condmap;
    }

  let run ~get_body_cb prepared =
    let open ScriptInterp in
    let match_bodymask id =
      Printf.printf "match_bodymask of %i\n" id;
      false
    in
    let match_bodymask = ext_fn "match_bodymask" ["int"] "bool" (Obj.repr match_bodymask) in
    let state = makeReadyCopy prepared.state in
    let world = add_external match_bodymask state.world in
    let state = { state with world } in
    interp state
end
