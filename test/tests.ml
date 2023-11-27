module TestUtils : sig
  val read_sample : string -> string

  val asset_with_file : string -> string -> unit
end = struct
  let read_sample filename =
    let channel = open_in ("../../../test/samples/" ^ filename) in
    let size = in_channel_length channel in
    let content = really_input_string channel size in
    close_in channel ; content

  let sample_exists filename =
    let path = "../../../test/samples/" ^ filename in
    Sys.file_exists path

  let write_sample_to_file filename content =
    let path = "../../../test/samples/" ^ filename in
    let oc = open_out path in
    output_string oc content ; close_out oc

  module TextComparer : sig
    val compare_2_txt : string -> string -> unit
  end = struct
    let save_string_to_temp_file string =
      let temp_file_path = Filename.temp_file "compare" "" in
      let oc = open_out temp_file_path in
      Fun.protect
        (fun _ -> output_string oc string ; temp_file_path)
        ~finally:(fun _ -> close_out oc)

    let compare_2_txt sample1 sample2 =
      let tmp1 = save_string_to_temp_file sample1 in
      let tmp2 = save_string_to_temp_file sample2 in
      let result = Sys.command @@ Printf.sprintf "diff -u %s %s" tmp1 tmp2 in
      if result <> 0 then failwith ""
  end

  let asset_with_file filename actual =
    if sample_exists filename then
      let expected = read_sample filename in
      TextComparer.compare_2_txt expected actual
    else write_sample_to_file filename actual
end

let () =
  TestUtils.read_sample "androidx-release-notes.xml"
  |> Compose_news.make_html_requests "2023-11-14T22:05:43.403Z"
  |> List.map (fun x -> Compose_news.rss_result_to_yojson x)
  |> fun xs ->
  Yojson.Safe.pretty_to_string (`List xs)
  |> TestUtils.asset_with_file "empty_rss_parse_sample.json"

let () =
  TestUtils.read_sample "androidx-release-notes.xml"
  |> Compose_news.make_html_requests "2023-11-16T22:05:43.403Z"
  |> List.map (fun x -> Compose_news.rss_result_to_yojson x)
  |> fun xs ->
  Yojson.Safe.pretty_to_string (`List xs)
  |> TestUtils.asset_with_file "empty_rss_parse_sample.json"

let () =
  let html_requests =
    TestUtils.read_sample "androidx-release-notes.xml"
    |> Compose_news.make_html_requests "2023-11-15T22:05:43.403Z"
  in
  (*  *)
  html_requests
  |> List.map (fun x -> Compose_news.rss_result_to_yojson x)
  |> fun xs ->
  Yojson.Safe.pretty_to_string (`List xs)
  |> TestUtils.asset_with_file "rss_parse_sample.json" ;
  (*  *)
  html_requests
  |> List.map (fun (x : Compose_news.rss_result) ->
         let path = (Hashtbl.hash x.link |> string_of_int) ^ ".html" in
         TestUtils.read_sample path )
  |> Compose_news.make_telegraph_post html_requests "34954e59d0af"
  |> TestUtils.asset_with_file "telegraph.json"

let () =
  Compose_news.make_telegram_body {|{"result":{"url":"14b44899113c"}}|}
    "203a0753e679"
  |> TestUtils.asset_with_file "send_telegram_request.json"
