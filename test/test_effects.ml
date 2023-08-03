module Command = struct
  type world = World

  type ('arg, 'result) eff_factory = {mutable action: 'arg -> 'result}

  type 'msg eff = world -> 'msg

  let stub () : ('arg, 'result) eff_factory =
    {action= (fun _ -> raise @@ Invalid_argument "Effect not implemented")}

  let call (arg : 'arg) (ef : ('arg, 'result) eff_factory) : 'result eff =
   fun (_ : world) -> ef.action arg

  type eff_with_handle = world -> unit

  let map (a : 'msg -> eff_with_handle) (b : 'msg eff) : eff_with_handle =
   fun w ->
    let _c = b w in
    let _d = a _c in
    _d w

  let apply (hs : 'msg list -> eff_with_handle) (fs : 'msg eff list) :
      eff_with_handle =
   fun w ->
    let _rs = List.map (fun f -> f w) fs in
    let _b = hs _rs in
    _b w

  let batch (fs : eff_with_handle list) : eff_with_handle =
   fun w -> fs |> List.iter (fun f -> f w)

  let empty : eff_with_handle = fun _ -> ()

  let attach_handler (eh : _ eff_factory) h = eh.action <- h
end

module MyEffects = struct
  type download_result = Download_result of string [@@deriving show]

  let download_cmd : (string, download_result) Command.eff_factory =
    Command.stub ()
end

open MyEffects

let __main (r : download_result list) : Command.eff_with_handle =
  print_endline @@ "__main with LIST [ "
  ^ List.fold_left (fun a x -> a ^ ", " ^ show_download_result x) "" r
  ^ " ]" ;
  Command.empty

let __main2 (r : download_result) : Command.eff_with_handle =
  print_endline @@ "__main with SINGLE | " ^ show_download_result r ;
  Command.empty

let _main =
  [ [ Command.call "https://g.com/foo1" download_cmd
    ; Command.call "https://g.com/foo2" download_cmd ]
    |> Command.apply __main
  ; Command.call "https://g.com/foo3" download_cmd |> Command.map __main2
  ; Command.call "https://g.com/foo4" download_cmd |> Command.map __main2 ]
  |> Command.batch

let _main_rss =
  download_cmd
  |> Command.call "https://g.com/updates.xml"
  |> Command.map __main2

let main () () =
  let log = ref [] in
  Command.attach_handler download_cmd (fun url ->
      log := url :: !log ;
      Download_result url ) ;
  _main Command.World ;
  !log |> List.fold_left (Printf.sprintf "%s\n- %s") "[LOG]" |> print_endline
