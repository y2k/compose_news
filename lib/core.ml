module StringMap = struct
  include Map.Make (String)

  let pp _ ppf m =
    Format.fprintf ppf "{" ;
    iter (fun k v -> Format.fprintf ppf "\"%s\": \"%s\"; " k v) m ;
    Format.fprintf ppf "}"
end

module ClearText = struct
  let translate = function
    | "Bug Fixes" ->
        "Исправление ошибок"
    | "API Changes" ->
        "Изменения API"
    | "Experimental K2 support" ->
        "Экспериментальная поддержка K2"
    | "Dependency Update" ->
        "Обновление зависимостей"
    | "New Features" ->
        "Новые функции"
    | text ->
        text

  let remove_issue_re = Re.Perl.compile_pat {| \([\w/, ]+\)|}

  let remove_issue input = Re.replace_string remove_issue_re input ~by:""
end

type http_msg_props = {env: string StringMap.t; body: string} [@@deriving show]

type req_props = ReqValue of string | ReqObj of (string * req_props) list
[@@deriving show]

type http_cmd_props =
  { url: string
  ; props: req_props
  ; callback: http_msg_props -> http_cmd_props list }
[@@deriving show]

let make_telegram_request (msg : http_msg_props) (new_message : string) =
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
  ; callback= (fun _ -> []) }

let on_http_downloaded (rss : Rss_parser.content) (msg : http_msg_props) =
  msg.body
  |> Html_parser.parse rss.version
  |> List.map (fun (x : Html_parser.item) ->
         x.content
         |> List.fold_left
              (fun state x -> state ^ "\n- " ^ ClearText.remove_issue x)
              ("[ " ^ ClearText.translate x.title ^ " ]") )
  |> List.fold_left (Printf.sprintf "%s\n%s") (rss.title ^ "\n")
  |> make_telegram_request msg
  |> fun x -> [x]

let compose_re = Re.str "ompose" |> Re.compile

let on_xml_downloaded msg =
  Rss_parser.main msg.body
  |> List.concat_map (fun (x : Rss_parser.item) -> x.links)
  |> List.filter (fun (x : Rss_parser.content) -> Re.execp compose_re x.title)
  |> List.filteri (fun i _ -> i < 2)
  |> List.map (fun (x : Rss_parser.content) ->
         {url= x.link; props= ReqObj []; callback= on_http_downloaded x} )

let on_scheduled =
  [ { url= "https://developer.android.com/feeds/androidx-release-notes.xml"
    ; props= ReqObj []
    ; callback= on_xml_downloaded } ]
