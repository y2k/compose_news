open Utils

module Commands = struct
  let download : (string * req_props, msg) Command.cmd = Command.stub ()
end

module Telegram = struct
  let make_telegram_request (env : env) (new_message : string) =
    let url =
      Printf.sprintf "https://api.telegram.org/bot%s/sendMessage" env.tg_token
    in
    let body =
      `Assoc [("chat_id", `String env.chat_id); ("text", `String new_message)]
      |> Yojson.Safe.to_string
    in
    let props =
      ReqObj
        [ ("body", ReqValue body)
        ; ("method", ReqValue "post")
        ; ("headers", ReqObj [("content-type", ReqValue "application/json")]) ]
    in
    Command.call (url, props) Commands.download
    |> Command.map (fun _ -> Command.empty)
end

let on_page_created (msg : msg) =
  Telegraph.get_page_url msg.body |> Telegram.make_telegram_request msg.env

let on_http_downloaded env (a : Rss_parser.content list) (b : msg list) =
  List.combine a b
  |> List.concat_map (fun ((rss : Rss_parser.content), msg) ->
         msg.body
         |> Html_parser.parse rss.version
         |> Fun.flip Telegraph.make_item_sample rss.title )
  |> Telegraph.add_meta
  |> Telegraph.create_request env.telegraph_token "Jetpack Compose updates"
  |> fun (url, props) ->
  Command.call (url, props) Commands.download |> Command.map on_page_created

let compose_re = Re.str "ompose" |> Re.compile

let on_xml_downloaded msg =
  Rss_parser.main msg.body
  (* |> List.filter (fun (x : Rss_parser.item) ->
         Date.parse_date x.date = msg.env.now ) *)
  (* |> List.filteri (fun i _ -> i < 1) *)
  |> List.filteri (fun i _ -> i = 1)
  |> List.concat_map (fun (x : Rss_parser.item) -> x.links)
  |> List.filter (fun (x : Rss_parser.content) -> Re.execp compose_re x.title)
  |> List.map (fun (x : Rss_parser.content) ->
         (x, Command.call (x.link, ReqObj []) Commands.download) )
  |> List.filteri (fun i _ -> i < 4)
  |> fun xs ->
  let a = List.map fst xs in
  List.map snd xs |> Command.sequence
  |> Command.map (on_http_downloaded msg.env a)

let on_scheduled _env =
  Commands.download
  |> Command.call
       ( "https://developer.android.com/feeds/androidx-release-notes.xml"
       , ReqObj [] )
  |> Command.map on_xml_downloaded
