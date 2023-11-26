type rss_result = {link: string; version: string; title: string}
[@@deriving yojson]

let make_html_requests (now_iso : string) xml : rss_result list =
  let composeRe = Re.Posix.compile_pat "compose" in
  let alphaRe = Re.Posix.compile_pat "-alpha" in
  let requests =
    Rss_parser.main xml
    |> List.filter (fun (x : Rss_parser.entry) ->
           let today_iso = String.sub now_iso 0 10 in
           String.starts_with ~prefix:today_iso x.updated )
    |> List.concat_map (fun (x : Rss_parser.entry) -> x.links)
    |> List.filter (fun (x : Rss_parser.content) ->
           Re.execp composeRe x.link && not (Re.execp alphaRe x.link) )
    |> List.map (fun (x : Rss_parser.content) ->
           {link= x.link; version= x.version; title= x.title} )
  in
  requests

let make_telegraph_post (rss_items : rss_result list) telegraph_token
    (htmls : string list) =
  let documents =
    rss_items
    |> List.map2
         (fun html (x : rss_result) -> (x, Html_parser.parse x.version html))
         htmls
  in
  documents
  |> List.map (fun (x, doc) -> Telegraph.make_item_sample doc x.title x.link)
  |> List.flatten |> Telegraph.add_meta
  |> Telegraph.add_contents (List.map (fun x -> x.title) rss_items)
  |> Telegraph.create_body telegraph_token "Обновления библиотек «Compose»"

let make_telegram_url tg_token =
  Printf.sprintf "https://api.telegram.org/bot%s/sendMessage" tg_token

let make_telegram_body response_json chat_id =
  let new_message = Telegraph.get_page_url response_json in
  `Assoc [("chat_id", `String chat_id); ("text", `String new_message)]
  |> Yojson.Safe.pretty_to_string
