open Js_of_ocaml
module U = Js_of_ocaml.Js.Unsafe
open Lib

let make_response (body : _) = U.new_obj U.global ##. Response [|U.inject body|]

let handle2 html =
  html
  |> Html_parser.parse "1.2.0-beta01"
  |> List.map Html_parser.show_item
  |> List.fold_left ( ^ ) "\n"

let handle xml =
  let links = Update_loader.main xml in
  (List.hd (List.hd links).links).link

let next f p = p##then_ f

let handle event =
  let url = "https://developer.android.com/feeds/androidx-release-notes.xml" in
  let p =
    Cloudflare.fetch__ url
    |> next handle
    |> next Cloudflare.fetch__
    |> next handle2
    |> next make_response in
  event##respondWith p

let () = U.global##addEventListener (U.inject "fetch") (Js.wrap_callback handle)
