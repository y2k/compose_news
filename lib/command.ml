type world = World | TestWorld

type 'result async_result = ('result -> unit) -> unit

type ('arg, 'result) cmd = {mutable action: 'arg -> 'result async_result}

type 'msg io = world -> 'msg async_result

let stub () : ('arg, 'result) cmd =
  {action= (fun _ -> raise @@ Invalid_argument "Effect not implemented")}

let call (arg : 'arg) (ef : ('arg, 'result) cmd) : 'result io =
 fun (_ : world) -> ef.action arg

let map (f : 'msg -> unit io) (io : 'msg io) : unit io =
 fun w _disp ->
  let _a = io w in
  _a (fun _b ->
      let _c = f _b in
      _c w ignore )

let sequence (io_list : 'msg io list) : 'msg list io =
 fun w _disp ->
  match w with
  | TestWorld ->
      io_list |> List.iter (fun _b -> _b w ignore)
  | World ->
      let rec rec_map (io_list : 'msg io list) (results : 'msg list) : unit =
        match io_list with
        | _x :: xs ->
            let _a = _x w in
            _a (fun _b -> rec_map xs (_b :: results))
        | [] ->
            _disp results
      in
      rec_map io_list []

let ignore_io (io : 'a io) : unit io = fun w disp -> io w ignore ; disp ()

let batch (fs : unit io list) : unit io = sequence fs |> ignore_io

let empty : unit io = fun _ d -> d ()

let attach_handler (eh : ('i, 'o) cmd) (h : 'i -> 'o) =
  eh.action <- (fun i d -> d (h i))

let attach_async_handler (eh : ('i, 'o) cmd) (h : 'i -> ('o -> unit) -> unit) =
  eh.action <- h
