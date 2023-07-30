open Lib
module Date = Utils.Date

(* Date time *)

let () =
  let actual = Date.parse_date "2023-07-26T00:00:00+00:00" in
  if actual <> Date.create 2023 7 26 then failwith "Invalid date"

(*  *)

type bar = {headers: int; foo: string; baz: string -> unit} [@@deriving show]

let () =
  let actual = show_bar {headers= 7; foo= "42"; baz= (fun _x -> ())} in
  if actual <> {|{ Tests.headers = 7; foo = "42"; baz = <fun> }|} then
    failwith actual

let read_sample_file filename =
  let ch = open_in_bin ("../../../test/samples/" ^ filename) in
  Fun.protect
    (fun _ -> really_input_string ch (in_channel_length ch))
    ~finally:(fun _ -> close_in ch)

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
    Sys.command @@ Printf.sprintf "diff -u %s %s" tmp1 tmp2 |> ignore
end

let () =
  let assert_date day _expected : unit =
    let actual =
      { env= {tg_token= ""; chat_id= ""; now= Date.create 2023 7 day}
      ; body= read_sample_file "rss2.xml" }
      |> Core.on_xml_downloaded |> List.map Core.show_cmd
      |> List.fold_left ( ^ ) ""
    in
    let expected = _expected |> Base64.decode |> Result.get_ok in
    if actual <> expected then (
      TextComparer.compare_2_txt expected actual ;
      (* TextComparer.compare_2_txt "bbb\nccc\nddd\n" "aaa\nbbb\nccc\n" ; *)
      prerr_endline @@ "========================\n"
      ^ Base64.encode_string actual
      ^ "\n========================" ;
      exit 1 )
  in
  assert_date 25 "" ;
  assert_date 26
    "eyBDb3JlLnVybCA9CiAgImh0dHBzOi8vZGV2ZWxvcGVyLmFuZHJvaWQuY29tL2pldHBhY2svYW5kcm9pZHgvcmVsZWFzZXMvY29tcG9zZS1hbmltYXRpb24jMS41LjAtcmMwMSI7CiAgcHJvcHMgPSAoQ29yZS5SZXFPYmogW10pOyBjYWxsYmFjayA9IDxmdW4+IH17IENvcmUudXJsID0KICAiaHR0cHM6Ly9kZXZlbG9wZXIuYW5kcm9pZC5jb20vamV0cGFjay9hbmRyb2lkeC9yZWxlYXNlcy9jb21wb3NlLWFuaW1hdGlvbiMxLjYuMC1hbHBoYTAyIjsKICBwcm9wcyA9IChDb3JlLlJlcU9iaiBbXSk7IGNhbGxiYWNrID0gPGZ1bj4gfXsgQ29yZS51cmwgPQogICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL2NvbXBvc2UtY29tcGlsZXIjMS41LjEiOwogIHByb3BzID0gKENvcmUuUmVxT2JqIFtdKTsgY2FsbGJhY2sgPSA8ZnVuPiB9eyBDb3JlLnVybCA9CiAgImh0dHBzOi8vZGV2ZWxvcGVyLmFuZHJvaWQuY29tL2pldHBhY2svYW5kcm9pZHgvcmVsZWFzZXMvY29tcG9zZS1mb3VuZGF0aW9uIzEuNS4wLXJjMDEiOwogIHByb3BzID0gKENvcmUuUmVxT2JqIFtdKTsgY2FsbGJhY2sgPSA8ZnVuPiB9eyBDb3JlLnVybCA9CiAgImh0dHBzOi8vZGV2ZWxvcGVyLmFuZHJvaWQuY29tL2pldHBhY2svYW5kcm9pZHgvcmVsZWFzZXMvY29tcG9zZS1mb3VuZGF0aW9uIzEuNi4wLWFscGhhMDIiOwogIHByb3BzID0gKENvcmUuUmVxT2JqIFtdKTsgY2FsbGJhY2sgPSA8ZnVuPiB9eyBDb3JlLnVybCA9CiAgImh0dHBzOi8vZGV2ZWxvcGVyLmFuZHJvaWQuY29tL2pldHBhY2svYW5kcm9pZHgvcmVsZWFzZXMvY29tcG9zZS1tYXRlcmlhbCMxLjUuMC1yYzAxIjsKICBwcm9wcyA9IChDb3JlLlJlcU9iaiBbXSk7IGNhbGxiYWNrID0gPGZ1bj4gfXsgQ29yZS51cmwgPQogICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL2NvbXBvc2UtbWF0ZXJpYWwjMS42LjAtYWxwaGEwMiI7CiAgcHJvcHMgPSAoQ29yZS5SZXFPYmogW10pOyBjYWxsYmFjayA9IDxmdW4+IH17IENvcmUudXJsID0KICAiaHR0cHM6Ly9kZXZlbG9wZXIuYW5kcm9pZC5jb20vamV0cGFjay9hbmRyb2lkeC9yZWxlYXNlcy9jb21wb3NlLW1hdGVyaWFsMyMxLjIuMC1hbHBoYTA0IjsKICBwcm9wcyA9IChDb3JlLlJlcU9iaiBbXSk7IGNhbGxiYWNrID0gPGZ1bj4gfXsgQ29yZS51cmwgPQogICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL2NvbXBvc2UtcnVudGltZSMxLjUuMC1yYzAxIjsKICBwcm9wcyA9IChDb3JlLlJlcU9iaiBbXSk7IGNhbGxiYWNrID0gPGZ1bj4gfXsgQ29yZS51cmwgPQogICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL2NvbXBvc2UtcnVudGltZSMxLjYuMC1hbHBoYTAyIjsKICBwcm9wcyA9IChDb3JlLlJlcU9iaiBbXSk7IGNhbGxiYWNrID0gPGZ1bj4gfXsgQ29yZS51cmwgPQogICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL2NvbXBvc2UtdWkjMS41LjAtcmMwMSI7CiAgcHJvcHMgPSAoQ29yZS5SZXFPYmogW10pOyBjYWxsYmFjayA9IDxmdW4+IH17IENvcmUudXJsID0KICAiaHR0cHM6Ly9kZXZlbG9wZXIuYW5kcm9pZC5jb20vamV0cGFjay9hbmRyb2lkeC9yZWxlYXNlcy9jb21wb3NlLXVpIzEuNi4wLWFscGhhMDIiOwogIHByb3BzID0gKENvcmUuUmVxT2JqIFtdKTsgY2FsbGJhY2sgPSA8ZnVuPiB9eyBDb3JlLnVybCA9CiAgImh0dHBzOi8vZGV2ZWxvcGVyLmFuZHJvaWQuY29tL2pldHBhY2svYW5kcm9pZHgvcmVsZWFzZXMvY29uc3RyYWludGxheW91dCMxLjEuMC1hbHBoYTExIjsKICBwcm9wcyA9IChDb3JlLlJlcU9iaiBbXSk7IGNhbGxiYWNrID0gPGZ1bj4gfXsgQ29yZS51cmwgPQogICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL3dlYXItY29tcG9zZSMxLjMuMC1hbHBoYTAyIjsKICBwcm9wcyA9IChDb3JlLlJlcU9iaiBbXSk7IGNhbGxiYWNrID0gPGZ1bj4gfXsgQ29yZS51cmwgPQogICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL3dlYXItY29tcG9zZSMxLjAuMC1hbHBoYTA4IjsKICBwcm9wcyA9IChDb3JlLlJlcU9iaiBbXSk7IGNhbGxiYWNrID0gPGZ1bj4gfXsgQ29yZS51cmwgPQogICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL3dlYXItY29tcG9zZSMxLjIuMC1yYzAxIjsKICBwcm9wcyA9IChDb3JlLlJlcU9iaiBbXSk7IGNhbGxiYWNrID0gPGZ1bj4gfQ==" ;
  assert_date 27 ""

let () =
  let rss = read_sample_file "sample3.html" in
  let cmds =
    Core.on_http_downloaded
      {title= "title"; link= "link"; version= "1.5.0-rc01"}
      { env=
          {tg_token= "TG_TOKEN"; chat_id= "CHAT_ID"; now= Date.create 2023 1 1}
      ; body= rss }
  in
  let actual = cmds |> List.map Core.show_cmd |> List.fold_left ( ^ ) "" in
  let expected =
    "eyBDb3JlLnVybCA9ICJodHRwczovL2FwaS50ZWxlZ3JhbS5vcmcvYm90VEdfVE9LRU4vc2VuZE1lc3NhZ2UiOwogIHByb3BzID0KICAoQ29yZS5SZXFPYmoKICAgICBbKCJib2R5IiwKICAgICAgIChDb3JlLlJlcVZhbHVlCiAgICAgICAgICAie1wiY2hhdF9pZFwiOlwiQ0hBVF9JRFwiLFwidGV4dFwiOlwidGl0bGVcXG5cXG5bIFwyMDhcMTUyXDIwOVwxMjlcMjA4XDE5MVwyMDlcMTI4XDIwOFwxNzZcMjA4XDE3OFwyMDhcMTg3XDIwOFwxODFcMjA4XDE4OVwyMDhcMTg0XDIwOFwxODEgXDIwOFwxOTBcMjA5XDEzNlwyMDhcMTg0XDIwOFwxNzdcMjA4XDE5MFwyMDhcMTg2IF1cXG4tIEZpeGVkIGFuIGlzc3VlIHdoZXJlIGNhbGxpbmcgLnZhbHVlIG9uIGEgcHJpbWl0aXZlIHN0YXRlIHR5cGUgd291bGQgcmVwb3J0IGEgbGludCB3YXJuaW5nIHdpdGggYW4gaW52YWxpZCBmaXguIFRoZSBpbnNwZWN0aW9uIHdpbGwgbm93IHJlY29tbWVuZCBtaWdyYXRpbmcgdG8gdGhlIGNvcnJlY3QgcHJvcGVydHkuXFxuLSBBbiBvcHRpb25hbCBpbnNwZWN0aW9uIHRvIHJlY29tbWVuZCBtaWdyYXRpbmcgbXV0YWJsZVN0YXRlT2YoKSBjYWxscyB0byB0aGVpciBjb3JyZXNwb25kaW5nIHNwZWNpYWxpemVkIHR5cGVzIGZvciBwcmltaXRpdmVzIGlzIGF2YWlsYWJsZS4gSXRzIGxpbnQgSUQgaXMgQXV0b2JveGluZ1N0YXRlQ3JlYXRpb24uIFByZXZpb3VzbHksIHRoaXMgaW5zcGVjdGlvbiB3YXMgZW5hYmxlZCBieSBkZWZhdWx0IGZvciBhbGwgcHJvamVjdHMuIFRvIHNlZSB0aGlzIHdhcm5pbmcgaW4gQW5kcm9pZCBTdHVkaW8ncyBlZGl0b3IgYW5kIHlvdXIgcHJvamVjdCdzIGxpbnQgb3V0cHV0cywgY2hhbmdlIGl0cyBzZXZlcml0eSBmcm9tIGluZm9ybWF0aW9uYWwgdG8gd2FybmluZyBieSBkZWNsYXJpbmcgd2FybmluZyBcXFwiQXV0b2JveGluZ1N0YXRlQ3JlYXRpb25cXFwiIGluc2lkZSB5b3VyIG1vZHVsZSdzIGJ1aWxkLmdyYWRsZSBvciBidWlsZC5ncmFkbGUua3RzIGNvbmZpZ3VyYXRpb24gYXMgc2hvd246ICAgICBhbmRyb2lkIHsgICAgICAgICBsaW50IHsgICAgICAgICAgICAgd2FybmluZyBcXFwiQXV0b2JveGluZ1N0YXRlQ3JlYXRpb25cXFwiICAgICAgICAgfSAgICAgICAgIC4uLiAgICAgfSBcIn0iKSk7CiAgICAgICAoIm1ldGhvZCIsIChDb3JlLlJlcVZhbHVlICJwb3N0IikpOwogICAgICAgKCJoZWFkZXJzIiwKICAgICAgICAoQ29yZS5SZXFPYmogWygiY29udGVudC10eXBlIiwgKENvcmUuUmVxVmFsdWUgImFwcGxpY2F0aW9uL2pzb24iKSldKSkKICAgICAgIF0pOwogIGNhbGxiYWNrID0gPGZ1bj4gfQ=="
    |> Base64.decode |> Result.get_ok
  in
  if actual <> expected then (
    prerr_endline (Base64.encode_string actual) ;
    failwith "" )

let () =
  let rss = read_sample_file "sample2.html" in
  let cmds =
    Core.on_http_downloaded
      {title= "title"; link= "link"; version= "1.1.0"}
      { env=
          {tg_token= "TG_TOKEN"; chat_id= "CHAT_ID"; now= Date.create 2023 1 1}
      ; body= rss }
  in
  let actual = cmds |> List.map Core.show_cmd |> List.fold_left ( ^ ) "" in
  let expected =
    "eyBDb3JlLnVybCA9ICJodHRwczovL2FwaS50ZWxlZ3JhbS5vcmcvYm90VEdfVE9LRU4vc2VuZE1lc3NhZ2UiOwogIHByb3BzID0KICAoQ29yZS5SZXFPYmoKICAgICBbKCJib2R5IiwKICAgICAgIChDb3JlLlJlcVZhbHVlCiAgICAgICAgICAie1wiY2hhdF9pZFwiOlwiQ0hBVF9JRFwiLFwidGV4dFwiOlwidGl0bGVcXG5cXG5bIEltcG9ydGFudCBjaGFuZ2VzIHNpbmNlIDEuMC4wIF1cXG4tIFN1cHBvcnQgZm9yIEpldHBhY2sgTWFjcm9iZW5jaG1hcmtzLCB3aGljaCBhbGxvd3MgeW91IHRvIG1lYXN1cmUgd2hvbGUtYXBwIGludGVyYWN0aW9ucyBsaWtlIHN0YXJ0dXAgYW5kIHNjcm9sbGluZywgcHJvdmlkZXMgdGhlIGFiaWxpdHkgdG8gY2FwdHVyZSB0cmFjZXMgICYgbWVhc3VyZSB0cmFjZSBzZWN0aW9ucy5cXG4tIFN1cHBvcnQgZm9yIEJhc2VsaW5lIFByb2ZpbGVzICAgQ29tcGlsYXRpb25Nb2RlLlBhcnRpYWwgdG8gbWVhc3VyZSB0aGUgZWZmZWN0aXZlbmVzcyBvZiBCYXNlbGluZSBQcm9maWxlcy4gQEJhc2VsaW5lUHJvZmlsZVJ1bGUgdG8gYXV0b21hdGljYWxseSBnZW5lcmF0ZSBCYXNlbGluZSBwcm9maWxlcyBmb3IgYSBnaXZlbiBjcml0aWNhbCB1c2VyIGpvdXJuZXkuIFxcbi0gU3VwcG9ydCBmb3IgQWxsb2NhdGlvbiBtZXRyaWNzICYgcHJvZmlsaW5nIGR1cmluZyBNaWNyb2JlbmNobWFyayBydW5zLlwifSIpKTsKICAgICAgICgibWV0aG9kIiwgKENvcmUuUmVxVmFsdWUgInBvc3QiKSk7CiAgICAgICAoImhlYWRlcnMiLAogICAgICAgIChDb3JlLlJlcU9iaiBbKCJjb250ZW50LXR5cGUiLCAoQ29yZS5SZXFWYWx1ZSAiYXBwbGljYXRpb24vanNvbiIpKV0pKQogICAgICAgXSk7CiAgY2FsbGJhY2sgPSA8ZnVuPiB9"
    |> Base64.decode |> Result.get_ok
  in
  if actual <> expected then (
    prerr_endline (Base64.encode_string actual) ;
    failwith "" )

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

let () =
  let remove_issue_re = Re.Perl.compile_pat {| \(\w+\)|} in
  let input = "aaa (I4aab5) bbb" in
  let actual = Re.replace_string remove_issue_re input ~by:"" in
  if actual <> "aaa bbb" then failwith actual

let () =
  let remove_issue_re = Re.Perl.compile_pat {| \([\w/, ]+\)|} in
  let input =
    {|- Change the use of ClosedFloatingPointRange for the lighter weight. (I4aab5)
- Added new Start alignment for FabPosition (Ib7aea, b/170592777)
- ModalBottomSheet respects local layout direction. (Ib4f44, b/285628622)|}
  in
  let expected =
    {|- Change the use of ClosedFloatingPointRange for the lighter weight.
- Added new Start alignment for FabPosition
- ModalBottomSheet respects local layout direction.|}
  in
  let actual = Re.replace_string remove_issue_re input ~by:"" in
  if actual <> expected then failwith ("\n" ^ actual ^ "\n")
