type world = World

type ('arg, 'result) cmd = {mutable action: 'arg -> 'result}

type 'msg io = world -> 'msg

let stub () : ('arg, 'result) cmd =
  {action= (fun _ -> raise @@ Invalid_argument "Effect not implemented")}

let call (arg : 'arg) (ef : ('arg, 'result) cmd) : 'result io =
 fun (_ : world) -> ef.action arg

let map (f : 'msg -> unit io) (io : 'msg io) : unit io =
 fun w ->
  let result = io w in
  let unit_result = f result in
  unit_result w

let sequence (io_list : 'msg io list) : 'msg list io =
 fun w -> io_list |> List.map (fun f -> f w)

let batch (fs : unit io list) : unit io =
 fun w -> fs |> List.iter (fun f -> f w)

let empty : unit io = fun _ -> ()

let attach_handler (eh : _ cmd) h = eh.action <- h
