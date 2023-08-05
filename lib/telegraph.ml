open Yojson.Safe
open Utils

let create_request access_token title content =
  let body =
    `Assoc
      [ ("access_token", `String access_token)
      ; ("title", `String title)
      ; ("author_name", `String "Compose News")
      ; ("author_url", `String "https://github.com/y2k/compose_news")
      ; ("content", `List content) ]
    |> Yojson.Safe.to_string
  in
  print_endline @@ "[LOG][SIZE]: " ^ string_of_int (String.length body) ;
  let url = "https://api.telegra.ph/createPage" in
  let props =
    ReqObj
      [ ("body", ReqValue body)
      ; ("method", ReqValue "post")
      ; ("headers", ReqObj [("content-type", ReqValue "application/json")]) ]
  in
  (url, props)

let get_page_url _resp_text =
  Yojson.Safe.from_string _resp_text
  |> Util.path ["result"; "url"]
  |> Option.get |> Util.to_string

let make_item_sample items title : Yojson.Safe.t list =
  items
  |> List.map (fun (x : Html_parser.item) ->
         [ (* `Assoc [("tag", `String "h3"); ("children", `List [`String title])] *)
           `Assoc
             [ ("tag", `String "h4")
             ; ("children", `List [`String (ClearText.translate x.title)]) ]
         ; `Assoc
             [ ("tag", `String "ul")
             ; ( "children"
               , `List
                   ( x.content
                   |> List.map (fun x ->
                          `Assoc
                            [ ("tag", `String "li")
                            ; ( "children"
                              , `List [`String (ClearText.clear_text x)] ) ] )
                   ) ) ]
         ; `Assoc [("tag", `String "hr")] ] )
  |> List.flatten
  |> fun xs ->
  `Assoc [("tag", `String "h3"); ("children", `List [`String title])] :: xs

let add_meta items : Yojson.Safe.t list =
  List.append items
    [ `Assoc
        [ ("tag", `String "a")
        ; ( "attrs"
          , `Assoc [("href", `String "https://github.com/y2k/compose_news")] )
        ; ("children", `List [`String "Powered by Compose News bot (github)"])
        ] ]
