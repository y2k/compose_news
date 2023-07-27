open Js_of_ocaml
open Lib.Core
open Cloudflare

let next f p = p##then_ f

let handle_scheduled event =
  let env = Cloudflare.make_env () in
  let rec handle_scheduled_ props =
    let download (prop : http_cmd_props) =
      prop |> Cloudflare.execute_request_
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
