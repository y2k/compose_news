open Cloudflare_worker
open Js_of_ocaml

let get_now_iso_string () = (new%js Js.date_now)##toISOString |> Js.to_string

let handle_scheduled () =
  let tg_token = Js.Unsafe.global ##. TG_TOKEN_ in
  let chat_id = Js.Unsafe.global ##. CHAT_ID_ in
  let telegraph_token : string = Js.Unsafe.global ##. TELEGRAPH_TOKEN_ in
  let open Promise.Syntax in
  let* xml =
    Fetch.fetch "https://developer.android.com/feeds/androidx-release-notes.xml"
      []
    >>= Response.text
  in
  let html_requests =
    Compose_news.make_html_requests (get_now_iso_string ()) xml
  in
  let* htmls =
    html_requests
    |> List.map (fun (x : Compose_news.rss_result) ->
           let+ html = Fetch.fetch x.link [] >>= Response.text in
           html )
    |> Promise.all_list
  in
  if List.length htmls = 0 then (
    print_endline @@ "LOG: No RSS updates for " ^ get_now_iso_string () ;
    Promise.return () )
  else
    let body =
      Compose_news.make_telegraph_post html_requests telegraph_token htmls
    in
    let* response_json =
      Fetch.fetch "https://api.telegra.ph/createPage"
        [ `Body body
        ; `Method "post"
        ; `Headers [("content-type", "application/json")] ]
      >>= Response.text
    in
    let* _ =
      Fetch.fetch
        (Compose_news.make_telegram_url tg_token)
        [ `Body (Compose_news.make_telegram_body response_json chat_id)
        ; `Method "post"
        ; `Headers [("content-type", "application/json")] ]
    in
    Promise.return ()

let () = CloudflareWorker.scheduled handle_scheduled
