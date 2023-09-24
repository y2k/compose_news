open Utils
open Utils.Common

module Commands = struct
  let download : (string * req_props, (msg, string) result) Command.cmd =
    Command.stub ()
end

module Telegram = struct
  let make_request env new_message =
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
    (url, props)

  let make_telegram_request (env : env) (new_message : string) =
    let url, props = make_request env new_message in
    Command.call (url, props) Commands.download
    |> Command.bind (fun _ ->
           print_endline @@ "[TELEGRAM] Message sended to server" ;
           (* Command.empty  *)
           fun _ d ->
             print_endline @@ "[TELEGRAM] END" ;
             d () )
end

let on_page_created_ msg =
  Telegraph.get_page_url msg.body |> Telegram.make_telegram_request msg.env

let on_page_created (msg : (msg, string) result) =
  match msg with Ok msg -> on_page_created_ msg | Error _ -> Command.empty

let on_http_downloaded env (a : Rss_parser.content list)
    (b : (msg, string) result list) =
  List.combine a b
  |> List.concat_map
       (fun ((rss : Rss_parser.content), (msg : (msg, string) result)) ->
         match msg with
         | Ok msg -> (
           (* print_endline @@ "[URL]: " ^ rss.link ; *)
           try
             msg.body
             |> Html_parser.parse rss.version
             |> Fun.flip Telegraph.make_item_sample rss.title
           with e ->
             (* let l = String.length msg.body in
                let html_end = String.sub msg.body (l - 30) 30 in *)
             let title = Common.get_title msg.body in
             print_endline @@ "[ERROR]: " ^ rss.link ^ "|" ^ title ;
             raise e )
         | Error _ ->
             [] )
  |> Telegraph.add_meta
  |> Telegraph.create_request env.telegraph_token "Jetpack Compose updates"
  |> fun (url, props) ->
  Command.call (url, props) Commands.download |> Command.bind on_page_created

let compose_re = Re.str "ompose" |> Re.compile

let on_xml_downloaded_ msg =
  Rss_parser.main msg.body
  (* FIXME *)
  |> List.filter (fun (x : Rss_parser.item) ->
         Date.parse_date x.date = msg.env.now )
  |> List.filteri (fun i _ -> i < 1)
  |> List.concat_map (fun (x : Rss_parser.item) -> x.links)
  |> List.filter (fun (x : Rss_parser.content) -> Re.execp compose_re x.title)
  (* FIXME *)
  (* |> List.filteri (fun i _ -> i < 2) *)
  |> List.filteri (fun i _ -> i < 10)
  |> List.map (fun (x : Rss_parser.content) ->
         (x, Command.call (x.link, ReqObj []) Commands.download) )
  |> fun xs ->
  if List.length xs = 0 then Command.empty
  else
    let a = List.map fst xs in
    List.map snd xs |> Command.sequence
    |> Command.bind (on_http_downloaded msg.env a)

(*  *)

let on_xml_downloaded (msg : (msg, string) result) =
  match msg with Ok msg -> on_xml_downloaded_ msg | Error _ -> Command.empty

let on_scheduled =
  Commands.download
  |> Command.call
       ( "https://developer.android.com/feeds/androidx-release-notes.xml"
       , ReqObj [] )
  |> Command.bind on_xml_downloaded
