open Js_of_ocaml
module U = Js_of_ocaml.Js.Unsafe
open Lib

let make_response (body : _) = U.new_obj U.global ##. Response [|U.inject body|]

let handle xml =
  Update_loader.main xml
  |> List.map Update_loader.show_item
  |> List.fold_left ( ^ ) "\n"

let handle event =
  let url = "https://developer.android.com/feeds/androidx-release-notes.xml" in
  Cloudflare.fetch_ url (fun promise ->
      event##respondWith
        (promise##then_ (fun xml ->
             let result = handle xml in
             make_response result ) ) )
  |> ignore

let () = U.global##addEventListener (U.inject "fetch") (Js.wrap_callback handle)
