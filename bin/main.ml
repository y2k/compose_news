open Js_of_ocaml
open Lib

let next f p = p##then_ f

open Cloudflare

let make_telegram_request (msg : http_msg_props) (new_message : string) =
  let module J = Yojson.Safe in
  let url =
    Printf.sprintf "https://api.telegram.org/bot%s/sendMessage"
      (StringMap.find "TG_TOKEN" msg.env)
  in
  let body =
    `Assoc
      [ ("chat_id", `String (StringMap.find "CHAT_ID" msg.env))
      ; ("text", `String new_message) ]
    |> Yojson.Safe.to_string
  in
  { url
  ; props=
      ReqObj
        [ ("body", ReqValue body)
        ; ("method", ReqValue "post")
        ; ("headers", ReqObj [("content-type", ReqValue "application-json")]) ]
  }

let on_html_downloaded in_msg html =
  let msg =
    let open Html_parser in
    html |> parse "1.2.0-beta01" |> List.map show_item
    |> List.fold_left ( ^ ) "\n"
  in
  make_telegram_request in_msg msg

let on_xml_downloaded xml =
  let links = Rss_parser.main xml in
  (List.hd (List.hd links).links).link

(* let handle_fetch event =
   let url = "https://developer.android.com/feeds/androidx-release-notes.xml" in
   let p =
     Cloudflare.fetch__ url |> next on_xml_downloaded |> next Cloudflare.fetch__
     |> next on_html_downloaded |> next make_response
   in
   event##respondWith p *)

let handle_scheduled event =
  print_endline "SCHEDULE called" ;
  let url = "https://developer.android.com/feeds/androidx-release-notes.xml" in
  let p =
    Cloudflare.fetch__ url |> next on_xml_downloaded |> next Cloudflare.fetch__
    |> next (on_html_downloaded (Cloudflare.make_env event))
    |> next Cloudflare.execute_request_
  in
  event##waitUntil p

(* let () = U.global##addEventListener (U.inject "fetch") (Js.wrap_callback handle) *)
module U = Js_of_ocaml.Js.Unsafe

let make_response (body : _) = U.new_obj U.global ##. Response [|U.inject body|]

let () =
  U.global##addEventListener (U.inject "scheduled")
    (Js.wrap_callback handle_scheduled)
