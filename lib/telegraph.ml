open Yojson.Safe

module ClearText : sig
  val unescape_html : string -> string
end = struct
  let clear_html_1_re = Re.str "&#34;" |> Re.compile

  let clear_html_2_re = Re.str "&#39;" |> Re.compile

  let clear_html_3_re = Re.str "&amp;" |> Re.compile

  let clear_html_4_re = Re.str "&quot;" |> Re.compile

  let remove_issue_re = Re.Perl.compile_pat {| \([\w/, ]+\)|}

  let unescape_html input =
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

module Translator = struct
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
    | "Known Issues" ->
        "Известные Проблемы"
    | text ->
        text
end

let create_body access_token title content =
  `Assoc
    [ ("access_token", `String access_token)
    ; ("title", `String title)
    ; ("author_name", `String "Compose News")
    ; ("author_url", `String "https://github.com/y2k/compose_news")
    ; ("content", `List content) ]
  |> Yojson.Safe.pretty_to_string

let create_request access_token title content =
  let body = create_body access_token title content in
  let url = "https://api.telegra.ph/createPage" in
  let props =
    `ReqObj
      [ ("body", `ReqValue body)
      ; ("method", `ReqValue "post")
      ; ("headers", `ReqObj [("content-type", `ReqValue "application/json")]) ]
  in
  (url, props)

let get_page_url resp_text =
  Yojson.Safe.from_string resp_text
  |> Util.path ["result"; "url"]
  |> Option.get |> Util.to_string

let tag name attrs children =
  `Assoc
    ( [("tag", `String name)]
    @ (if List.length attrs == 0 then [] else [("attrs", `Assoc attrs)])
    @ if List.length children == 0 then [] else [("children", `List children)]
    )

let add_contents (titles : string list) htmls : Yojson.Safe.t list =
  [tag "h3" [] [`String "Оглавление"]]
  @ [ tag "ul" []
        ( titles
        |> List.map (fun title ->
               tag "li" []
                 [ tag "a"
                     [ ( "href"
                       , `String
                           ( "#"
                           ^ String.map
                               (fun c -> if c = ' ' then '-' else c)
                               title ) ) ]
                     [`String title] ] ) ) ]
  @ htmls

let make_item_sample (document : Html_parser.document) title url :
    Yojson.Safe.t list =
  document.nodes
  |> List.map (fun (x : Html_parser.item) ->
         [ tag "h4" [] [`String (Translator.translate x.title)]
         ; tag "ul" []
             ( x.content
             |> List.map (fun x ->
                    tag "li" [] [`String (ClearText.unescape_html x)] ) )
         ; tag "hr" [] [] ] )
  |> List.flatten
  |> fun xs ->
  tag "a" [("href", `String url)] [tag "h3" [] [`String title]] :: xs

let add_meta items : Yojson.Safe.t list =
  List.append items
    [ tag "a"
        [("href", `String "https://github.com/y2k/compose_news")]
        [`String "Powered by Compose News bot (github)"] ]
