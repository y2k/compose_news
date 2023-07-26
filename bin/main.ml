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
        ; ("headers", ReqObj [("content-type", ReqValue "application/json")]) ]
  }

let translate = function
  | "Bug Fixes" ->
      "Исправление ошибок"
  | "API Changes" ->
      "Изменения API"
  | "Experimental K2 support" ->
      "Экспериментальная поддержка K2"
  | text ->
      text

let on_html_downloaded (rss : Rss_parser.content) meta html =
  let items = html |> Html_parser.parse rss.version in
  let msg =
    items
    |> List.map (fun (x : Html_parser.item) ->
           x.content
           |> List.fold_left
                (fun a b -> a ^ "\n- " ^ b)
                ("[ " ^ translate x.title ^ " ]") )
    |> List.fold_left (Printf.sprintf "%s\n%s") (rss.title ^ "\n")
  in
  make_telegram_request meta msg

let on_xml_downloaded xml =
  let links = Rss_parser.main xml in
  links
  |> List.concat_map (fun (x : Rss_parser.item) -> x.links)
  |> Array.of_list

let handle_scheduled event =
  let url = "https://developer.android.com/feeds/androidx-release-notes.xml" in
  let p =
    Cloudflare.fetch__ url
    |> next (fun xml ->
           let rssArray = on_xml_downloaded xml in
           rssArray
           |> Array.map (fun rss ->
                  Cloudflare.fetch__ rss.link
                  |> next (on_html_downloaded rss (Cloudflare.make_env event))
                  |> next Cloudflare.execute_request_ )
           |> Js.array
           |> fun pall -> U.global ##. Promise##all pall )
  in
  event##waitUntil p

(* let () = U.global##addEventListener (U.inject "fetch") (Js.wrap_callback handle) *)
(* let make_response (body : _) = U.new_obj U.global ##. Response [|U.inject body|] *)

let () =
  let module U = Js_of_ocaml.Js.Unsafe in
  U.global##addEventListener (U.inject "scheduled")
    (Js.wrap_callback handle_scheduled)
