open Js_of_ocaml
module Unsafe = Js.Unsafe
open Lib
open Lib.Utils

module Utils = struct
  let delay seconds =
    Promise.make (fun ~resolve ~reject:_ ->
        Js_of_ocaml.Dom_html.setTimeout resolve (seconds *. 1000.0) |> ignore )
end

let execute_request (url : string) props =
  let rec mk_req = function
    | ReqObj props ->
        Unsafe.obj
          (Array.of_list (List.map (fun (k, p) -> (k, mk_req p)) props))
    | ReqValue v ->
        Js.string v |> Unsafe.inject
  in
  Unsafe.global##fetch (Unsafe.inject url) (mk_req props)

let get_today () =
  {|(function() { let now = new Date(); return now.getFullYear() + "-" + (now.getMonth() + 1) + "-" + now.getDate() + "T00:00:00+00:00" })()|}
  |> Js.Unsafe.js_expr |> Js.to_string |> Lib.Utils.Date.parse_date

let make_env () : env =
  { tg_token= Unsafe.global ##. TG_TOKEN_
  ; chat_id= Unsafe.global ##. CHAT_ID_
  ; telegraph_token= Unsafe.global ##. TELEGRAPH_TOKEN_
  ; now= get_today () }

let handle_scheduled _ =
  Promise.make (fun ~resolve ~reject:_ ->
      Core.on_scheduled (make_env ()) World (fun _ -> resolve [||]) )

let () =
  Command.attach_async_handler Core.Commands.download
    (fun (url, props) dispatch ->
      execute_request url props
      |> Promise.then_ ~fulfilled:(fun response -> response##text)
      |> Promise.then_ ~fulfilled:(fun text ->
             Ok {body= text; env= make_env ()} |> dispatch |> Promise.return )
      |> ignore ) ;
  Js.Unsafe.global##addEventListener
    (Js.Unsafe.inject "scheduled")
    (Js.wrap_callback (fun e -> e##waitUntil (handle_scheduled e)))
