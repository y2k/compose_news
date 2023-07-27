open Js_of_ocaml
module Unsafe = Js.Unsafe
open Lib.Core

let execute_request (cmd : cmd) =
  let rec mk_req = function
    | ReqObj props ->
        Unsafe.obj
          (Array.of_list (List.map (fun (k, p) -> (k, mk_req p)) props))
    | ReqValue v ->
        Js.string v |> Unsafe.inject
  in
  Unsafe.global##fetch (Unsafe.inject cmd.url) (mk_req cmd.props)

let make_env () : string StringMap.t =
  List.to_seq
    [ ("TG_TOKEN", Unsafe.global ##. TG_TOKEN_)
    ; ("CHAT_ID", Unsafe.global ##. CHAT_ID_) ]
  |> StringMap.of_seq

let next f p = p##then_ f

let handle_scheduled event =
  let env = make_env () in
  let rec handle_scheduled_ props =
    let download (prop : cmd) =
      prop |> execute_request
      |> next (fun response -> response##text)
      |> next (fun text ->
             prop.callback {body= text; env} |> handle_scheduled_ )
    in
    props |> List.map download |> Array.of_list
    |> fun pall -> Js.Unsafe.global ##. Promise##all pall
  in
  handle_scheduled_ on_scheduled |> fun p -> event##waitUntil p

let () =
  Js.Unsafe.global##addEventListener
    (Js.Unsafe.inject "scheduled")
    (Js.wrap_callback handle_scheduled)
