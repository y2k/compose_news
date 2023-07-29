module StringMap = struct
  include Map.Make (String)

  let pp _ ppf m =
    Format.fprintf ppf "{" ;
    iter (Format.fprintf ppf "\"%s\": \"%s\"; ") m ;
    Format.fprintf ppf "}"
end

module ClearText : sig
  val translate : string -> string

  val clear_text : string -> string
end = struct
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

  let clear_html_1_re = Re.str "&#34;" |> Re.compile

  let clear_html_2_re = Re.str "&#39;" |> Re.compile

  let clear_html_3_re = Re.str "&amp;" |> Re.compile

  let clear_html_4_re = Re.str "&quot;" |> Re.compile

  let remove_issue_re = Re.Perl.compile_pat {| \([\w/, ]+\)|}

  let clear_text input =
    let decode_html text =
      text
      |> Re.replace_string clear_html_1_re ~by:"\""
      |> Re.replace_string clear_html_2_re ~by:"'"
      |> Re.replace_string clear_html_3_re ~by:"&"
      |> Re.replace_string clear_html_4_re ~by:"\""
    in
    let remove_issue input = Re.replace_string remove_issue_re input ~by:"" in
    input |> remove_issue |> decode_html
end

type msg = {env: string StringMap.t; body: string} [@@deriving show]

type req_props = ReqValue of string | ReqObj of (string * req_props) list
[@@deriving show]

type cmd = {url: string; props: req_props; callback: msg -> cmd list}
[@@deriving show]

let make_telegram_request env (new_message : string) =
  let url =
    Printf.sprintf "https://api.telegram.org/bot%s/sendMessage"
      (StringMap.find "TG_TOKEN" env)
  in
  let body =
    `Assoc
      [ ("chat_id", `String (StringMap.find "CHAT_ID" env))
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

let on_http_downloaded (rss : Rss_parser.content) (msg : msg) =
  msg.body
  |> Html_parser.parse rss.version
  |> List.map (fun (x : Html_parser.item) ->
         x.content
         |> List.fold_left
              (fun state x -> state ^ "\n- " ^ ClearText.clear_text x)
              ("[ " ^ ClearText.translate x.title ^ " ]") )
  |> List.fold_left (Printf.sprintf "%s\n%s") (rss.title ^ "\n")
  |> make_telegram_request msg.env
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
