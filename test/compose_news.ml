let read_sample_file filename =
  let ch = open_in_bin ("../../../test/samples/" ^ filename) in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

open Lib

let () =
  let get_new_substring prefix html =
    let r1 = Re.str ({|<h3 id="|} ^ prefix ^ "\"") |> Re.compile in
    let start_pos = Re.Group.start (Re.exec r1 html) 0 in
    let r2 = Re.str "<h3" |> Re.compile in
    let end_pos = Re.Group.start (Re.exec ~pos:(start_pos + 1) r2 html) 0 in
    String.sub html start_pos (end_pos - start_pos)
    |> Printf.sprintf "<div>%s</div>"
  in
  let actual =
    get_new_substring "1.2.0-beta01"
      {|xxx yyy<h3 id="1.2.0-beta01">aaa bbb</h3>xxx ddd<h3 id="1.2.0-beta00>ssdfd</h3>"|}
  in
  if actual <> {|<div><h3 id="1.2.0-beta01">aaa bbb</h3>xxx ddd</div>|} then
    failwith actual

let () =
  let open Html_parser in
  let assert_ expr str =
    ( match parse_partial expr str with
    | Ok v ->
        v
    | Error msg ->
        failwith @@ "'" ^ str ^ "' " ^ msg )
    |> ignore
  in
  assert_ pstart_tag "<div>" ;
  assert_ pstart_tag "<div  a1=\"b2\">" ;
  assert_ pstart_tag "<div  k1=\"v1\"  k2=\"v2\">" ;
  assert_ pend_tag "</div>" ;
  assert_ ptext "xxx" ;
  assert_ presult "<div k1=\"v1\" k2=\"v2\"></div>" ;
  assert_ presult "<div>b</div>" ;
  assert_ presult "<div><div></div></div>" ;
  assert_ presult "<div>  <div>  </div>  </div>" ;
  assert_ presult "<div>a<div></div></div>" ;
  assert_ presult "<div>  a  <div>  b  </div>  c  </div>" ;
  assert_ presult "<div><div>a</div></div>" ;
  assert_ presult "<div><div></div>a</div>" ;
  assert_ presult "<div>a<div></div>b</div>" ;
  assert_ presult
    "<div k1=\"v1\" k2=\"v2\">a<div k1=\"v1\" k2=\"v2\">c</div>b</div>"

let () =
  (* RSS *)
  read_sample_file "rss.xml" |> Rss_parser.main
  |> List.map Rss_parser.show_item
  |> List.fold_left ( ^ ) "\n" |> ignore ;
  (* HTML *)
  read_sample_file "sample2.html"
  |> Html_parser.parse "1.2.0-beta01"
  |> List.map Html_parser.show_item
  |> List.fold_left ( ^ ) "\n" |> ignore

let () =
  let compose_re = Re.str "ompose" |> Re.compile in
  let assert_ x = if not @@ Re.execp compose_re x then failwith x in
  assert_ "compose" ;
  assert_ "Compose" ;
  assert_ "Foo Compose Bar" ;
  assert_ "Foo compose Bar"
