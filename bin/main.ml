open Js_of_ocaml
module Unsafe = Js.Unsafe
open Lib
open Lib.Core
open Lib.Utils

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

let next f p = p##then_ f

let catch f p = p##catch f

let handle_scheduled event =
  let promise =
    Unsafe.new_obj Unsafe.global ##. Promise
      [| Unsafe.inject
           (Js.wrap_callback (fun resolve _b ->
                Core.on_scheduled (make_env ()) World (fun _ ->
                    Unsafe.fun_call resolve [||] |> ignore ) ) ) |]
  in
  event##waitUntil promise

let () =
  Command.attach_async_handler Core.Commands.download
    (fun (url, props) dispatch ->
      (* print_endline @@ "[LOG] BEFORE execute_request, " ^ url ; *)
      execute_request url props
      |> next (fun response -> response##text)
      |> next (fun text ->
             print_endline @@ "[LOG] AFTER execute_request, " ^ url ;
             dispatch @@ Ok {body= text; env= make_env ()} ) ) ;
  Js.Unsafe.global##addEventListener
    (Js.Unsafe.inject "scheduled")
    (Js.wrap_callback handle_scheduled)
