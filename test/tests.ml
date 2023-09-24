open Lib
open Utils
open Alcotest
module Date = Utils.Common.Date
open Utils.Common

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

(* let assert_and_show_diff actual expected =
   if actual <> expected then (
     TextComparer.compare_2_txt expected actual ;
     prerr_endline @@ "========================\n"
     ^ Base64.encode_string actual
     ^ "\n========================" ;
     failwith "" |> ignore
      ) *)

let assert_and_show_diff actual expected =
  if actual <> expected then (
    TextComparer.compare_2_txt expected actual ;
    prerr_endline @@ "========================\n"
    ^ Base64.encode_string actual
    ^ "\n========================" ) ;
  check string "" expected actual

let get_actual (file_name : string) version : string =
  let logs : _ list ref = ref [] in
  Command.attach_async_handler Core.Commands.download (fun (url, props) _ ->
      let rec serialize_props (p : req_props) =
        match p with
        | ReqValue x ->
            `String x
        | ReqObj xs ->
            `Assoc (xs |> List.map (fun (k, v) -> (k, serialize_props v)))
      in
      let ji =
        `Assoc [("url", `String url); ("props", serialize_props props)]
        |> Yojson.Safe.pretty_to_string
      in
      logs := ji :: !logs ) ;
  Core.on_http_downloaded
    { tg_token= "TG_TOKEN"
    ; chat_id= "CHAT_ID"
    ; telegraph_token= "TELEGRAPH_TOKEN"
    ; now= Date.create 2023 1 1 }
    [{title= "title"; link= "link"; version}]
    [ Ok
        { env=
            { tg_token= "TG_TOKEN"
            ; chat_id= "CHAT_ID"
            ; telegraph_token= "TELEGRAPH_TOKEN"
            ; now= Date.create 2023 1 1 }
        ; body= read_sample_file file_name } ]
  |> fun f ->
  f Command.TestWorld ignore ;
  !logs |> List.fold_left ( ^ ) ""

let get_actual_2 (file_name : string) day : string =
  let logs : _ list ref = ref [] in
  Command.attach_async_handler Core.Commands.download (fun (url, props) _ ->
      let rec serialize_props (p : req_props) =
        match p with
        | ReqValue x ->
            `String x
        | ReqObj xs ->
            `Assoc (xs |> List.map (fun (k, v) -> (k, serialize_props v)))
      in
      let ji =
        `Assoc [("url", `String url); ("props", serialize_props props)]
        |> Yojson.Safe.pretty_to_string
      in
      logs := ji :: !logs ) ;
  Core.on_xml_downloaded
    (Ok
       { env=
           { tg_token= "TG_TOKEN"
           ; chat_id= "CHAT_ID"
           ; telegraph_token= "TELEGRAPH_TOKEN"
           ; now= Date.create 2023 7 day }
       ; body= read_sample_file file_name } )
  |> fun f ->
  f Command.TestWorld ignore ;
  !logs |> List.fold_left ( ^ ) ""

let () =
  let assert_date day expected : unit =
    let actual = get_actual_2 "rss2.xml" day in
    assert_and_show_diff actual (expected |> Base64.decode |> Result.get_ok)
  in
  assert_date 25 "" ;
  assert_date 26
    "ewogICJ1cmwiOiAiaHR0cHM6Ly9kZXZlbG9wZXIuYW5kcm9pZC5jb20vamV0cGFjay9hbmRyb2lkeC9yZWxlYXNlcy9jb21wb3NlLXJ1bnRpbWUjMS42LjAtYWxwaGEwMiIsCiAgInByb3BzIjoge30KfXsKICAidXJsIjogImh0dHBzOi8vZGV2ZWxvcGVyLmFuZHJvaWQuY29tL2pldHBhY2svYW5kcm9pZHgvcmVsZWFzZXMvY29tcG9zZS1ydW50aW1lIzEuNS4wLXJjMDEiLAogICJwcm9wcyI6IHt9Cn17CiAgInVybCI6ICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL2NvbXBvc2UtbWF0ZXJpYWwzIzEuMi4wLWFscGhhMDQiLAogICJwcm9wcyI6IHt9Cn17CiAgInVybCI6ICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL2NvbXBvc2UtbWF0ZXJpYWwjMS42LjAtYWxwaGEwMiIsCiAgInByb3BzIjoge30KfXsKICAidXJsIjogImh0dHBzOi8vZGV2ZWxvcGVyLmFuZHJvaWQuY29tL2pldHBhY2svYW5kcm9pZHgvcmVsZWFzZXMvY29tcG9zZS1tYXRlcmlhbCMxLjUuMC1yYzAxIiwKICAicHJvcHMiOiB7fQp9ewogICJ1cmwiOiAiaHR0cHM6Ly9kZXZlbG9wZXIuYW5kcm9pZC5jb20vamV0cGFjay9hbmRyb2lkeC9yZWxlYXNlcy9jb21wb3NlLWZvdW5kYXRpb24jMS42LjAtYWxwaGEwMiIsCiAgInByb3BzIjoge30KfXsKICAidXJsIjogImh0dHBzOi8vZGV2ZWxvcGVyLmFuZHJvaWQuY29tL2pldHBhY2svYW5kcm9pZHgvcmVsZWFzZXMvY29tcG9zZS1mb3VuZGF0aW9uIzEuNS4wLXJjMDEiLAogICJwcm9wcyI6IHt9Cn17CiAgInVybCI6ICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL2NvbXBvc2UtY29tcGlsZXIjMS41LjEiLAogICJwcm9wcyI6IHt9Cn17CiAgInVybCI6ICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL2NvbXBvc2UtYW5pbWF0aW9uIzEuNi4wLWFscGhhMDIiLAogICJwcm9wcyI6IHt9Cn17CiAgInVybCI6ICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL2NvbXBvc2UtYW5pbWF0aW9uIzEuNS4wLXJjMDEiLAogICJwcm9wcyI6IHt9Cn0=" ;
  assert_date 27 ""

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

let f2 () =
  let remove_issue_re = Re.Perl.compile_pat {| \(\w+\)|} in
  let input = "aaa (I4aab5) bbb" in
  let actual = Re.replace_string remove_issue_re input ~by:"" in
  (* if actual <> "aaa bbb" then failwith actual *)
  check string "[4]" "aaa bbb" actual

let f1 () =
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
  (* if actual <> expected then failwith ("\n" ^ actual ^ "\n") *)
  check string "[4]" expected actual

let f3 =
  let compose_re = Re.str "ompose" |> Re.compile in
  let assert_ x =
    let a () = check bool x true (Re.execp compose_re x) in
    test_case x `Quick a
  in
  [ assert_ "compose"
  ; assert_ "Compose"
  ; assert_ "Foo Compose Bar"
  ; assert_ "Foo compose Bar" ]

let f4 =
  let open Html_parser in
  let assert_ expr str =
    let actual =
      match parse_partial expr str with
      | Ok _ ->
          ""
      | Error msg ->
          "'" ^ str ^ "' " ^ msg
    in
    let a () = check string "???" "" actual in
    test_case "???" `Quick a
    (* ( match parse_partial expr str with
       | Ok v ->
           v
       | Error msg ->
           failwith @@ "'" ^ str ^ "' " ^ msg )
       |> ignore *)
  in
  [ assert_ pstart_tag "<div>"
  ; assert_ pstart_tag "<div  a1=\"b2\">"
  ; assert_ pstart_tag "<div  k1=\"v1\"  k2=\"v2\">"
  ; assert_ pend_tag "</div>"
  ; assert_ ptext "xxx"
  ; assert_ presult "<div k1=\"v1\" k2=\"v2\"></div>"
  ; assert_ presult "<div>b</div>"
  ; assert_ presult "<div><div></div></div>"
  ; assert_ presult "<div>  <div>  </div>  </div>"
  ; assert_ presult "<div>a<div></div></div>"
  ; assert_ presult "<div>  a  <div>  b  </div>  c  </div>"
  ; assert_ presult "<div><div>a</div></div>"
  ; assert_ presult "<div><div></div>a</div>"
  ; assert_ presult "<div>a<div></div>b</div>"
  ; assert_ presult
      "<div k1=\"v1\" k2=\"v2\">a<div k1=\"v1\" k2=\"v2\">c</div>b</div>" ]

let f5 () =
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
  let expected = {|<div><h3 id="1.2.0-beta01">aaa bbb</h3>xxx ddd</div>|} in
  check string "???" expected actual
(* if actual <> expected then
   failwith actual *)

let f7 () =
  let actual = get_actual "sample3.html" "1.5.0-rc01" in
  let expected =
    "ewogICJ1cmwiOiAiaHR0cHM6Ly9hcGkudGVsZWdyYS5waC9jcmVhdGVQYWdlIiwKICAicHJvcHMiOiB7CiAgICAiYm9keSI6ICJ7XCJhY2Nlc3NfdG9rZW5cIjpcIlRFTEVHUkFQSF9UT0tFTlwiLFwidGl0bGVcIjpcIkpldHBhY2sgQ29tcG9zZSB1cGRhdGVzXCIsXCJhdXRob3JfbmFtZVwiOlwiQ29tcG9zZSBOZXdzXCIsXCJhdXRob3JfdXJsXCI6XCJodHRwczovL2dpdGh1Yi5jb20veTJrL2NvbXBvc2VfbmV3c1wiLFwiY29udGVudFwiOlt7XCJ0YWdcIjpcImgzXCIsXCJjaGlsZHJlblwiOltcInRpdGxlXCJdfSx7XCJ0YWdcIjpcImg0XCIsXCJjaGlsZHJlblwiOltcItCY0YHQv9GA0LDQstC70LXQvdC40LUg0L7RiNC40LHQvtC6XCJdfSx7XCJ0YWdcIjpcInVsXCIsXCJjaGlsZHJlblwiOlt7XCJ0YWdcIjpcImxpXCIsXCJjaGlsZHJlblwiOltcIkZpeGVkIGFuIGlzc3VlIHdoZXJlIGNhbGxpbmcgLnZhbHVlIG9uIGEgcHJpbWl0aXZlIHN0YXRlIHR5cGUgd291bGQgcmVwb3J0IGEgbGludCB3YXJuaW5nIHdpdGggYW4gaW52YWxpZCBmaXguIFRoZSBpbnNwZWN0aW9uIHdpbGwgbm93IHJlY29tbWVuZCBtaWdyYXRpbmcgdG8gdGhlIGNvcnJlY3QgcHJvcGVydHkuXCJdfSx7XCJ0YWdcIjpcImxpXCIsXCJjaGlsZHJlblwiOltcIkFuIG9wdGlvbmFsIGluc3BlY3Rpb24gdG8gcmVjb21tZW5kIG1pZ3JhdGluZyBtdXRhYmxlU3RhdGVPZigpIGNhbGxzIHRvIHRoZWlyIGNvcnJlc3BvbmRpbmcgc3BlY2lhbGl6ZWQgdHlwZXMgZm9yIHByaW1pdGl2ZXMgaXMgYXZhaWxhYmxlLiBJdHMgbGludCBJRCBpcyBBdXRvYm94aW5nU3RhdGVDcmVhdGlvbi4gUHJldmlvdXNseSwgdGhpcyBpbnNwZWN0aW9uIHdhcyBlbmFibGVkIGJ5IGRlZmF1bHQgZm9yIGFsbCBwcm9qZWN0cy4gVG8gc2VlIHRoaXMgd2FybmluZyBpbiBBbmRyb2lkIFN0dWRpbydzIGVkaXRvciBhbmQgeW91ciBwcm9qZWN0J3MgbGludCBvdXRwdXRzLCBjaGFuZ2UgaXRzIHNldmVyaXR5IGZyb20gaW5mb3JtYXRpb25hbCB0byB3YXJuaW5nIGJ5IGRlY2xhcmluZyB3YXJuaW5nIFxcXCJBdXRvYm94aW5nU3RhdGVDcmVhdGlvblxcXCIgaW5zaWRlIHlvdXIgbW9kdWxlJ3MgYnVpbGQuZ3JhZGxlIG9yIGJ1aWxkLmdyYWRsZS5rdHMgY29uZmlndXJhdGlvbiBhcyBzaG93bjogICAgIGFuZHJvaWQgeyAgICAgICAgIGxpbnQgeyAgICAgICAgICAgICB3YXJuaW5nIFxcXCJBdXRvYm94aW5nU3RhdGVDcmVhdGlvblxcXCIgICAgICAgICB9ICAgICAgICAgLi4uICAgICB9IFwiXX1dfSx7XCJ0YWdcIjpcImhyXCJ9LHtcInRhZ1wiOlwiYVwiLFwiYXR0cnNcIjp7XCJocmVmXCI6XCJodHRwczovL2dpdGh1Yi5jb20veTJrL2NvbXBvc2VfbmV3c1wifSxcImNoaWxkcmVuXCI6W1wiUG93ZXJlZCBieSBDb21wb3NlIE5ld3MgYm90IChnaXRodWIpXCJdfV19IiwKICAgICJtZXRob2QiOiAicG9zdCIsCiAgICAiaGVhZGVycyI6IHsgImNvbnRlbnQtdHlwZSI6ICJhcHBsaWNhdGlvbi9qc29uIiB9CiAgfQp9"
    |> Base64.decode |> Result.get_ok
  in
  assert_and_show_diff actual expected

let f6 () =
  let actual = get_actual "sample2.html" "1.1.0" in
  let expected =
    "ewogICJ1cmwiOiAiaHR0cHM6Ly9hcGkudGVsZWdyYS5waC9jcmVhdGVQYWdlIiwKICAicHJvcHMiOiB7CiAgICAiYm9keSI6ICJ7XCJhY2Nlc3NfdG9rZW5cIjpcIlRFTEVHUkFQSF9UT0tFTlwiLFwidGl0bGVcIjpcIkpldHBhY2sgQ29tcG9zZSB1cGRhdGVzXCIsXCJhdXRob3JfbmFtZVwiOlwiQ29tcG9zZSBOZXdzXCIsXCJhdXRob3JfdXJsXCI6XCJodHRwczovL2dpdGh1Yi5jb20veTJrL2NvbXBvc2VfbmV3c1wiLFwiY29udGVudFwiOlt7XCJ0YWdcIjpcImgzXCIsXCJjaGlsZHJlblwiOltcInRpdGxlXCJdfSx7XCJ0YWdcIjpcImg0XCIsXCJjaGlsZHJlblwiOltcIkltcG9ydGFudCBjaGFuZ2VzIHNpbmNlIDEuMC4wXCJdfSx7XCJ0YWdcIjpcInVsXCIsXCJjaGlsZHJlblwiOlt7XCJ0YWdcIjpcImxpXCIsXCJjaGlsZHJlblwiOltcIlN1cHBvcnQgZm9yIEpldHBhY2sgTWFjcm9iZW5jaG1hcmtzLCB3aGljaCBhbGxvd3MgeW91IHRvIG1lYXN1cmUgd2hvbGUtYXBwIGludGVyYWN0aW9ucyBsaWtlIHN0YXJ0dXAgYW5kIHNjcm9sbGluZywgcHJvdmlkZXMgdGhlIGFiaWxpdHkgdG8gY2FwdHVyZSB0cmFjZXMgICYgbWVhc3VyZSB0cmFjZSBzZWN0aW9ucy5cIl19LHtcInRhZ1wiOlwibGlcIixcImNoaWxkcmVuXCI6W1wiU3VwcG9ydCBmb3IgQmFzZWxpbmUgUHJvZmlsZXMgICBDb21waWxhdGlvbk1vZGUuUGFydGlhbCB0byBtZWFzdXJlIHRoZSBlZmZlY3RpdmVuZXNzIG9mIEJhc2VsaW5lIFByb2ZpbGVzLiBAQmFzZWxpbmVQcm9maWxlUnVsZSB0byBhdXRvbWF0aWNhbGx5IGdlbmVyYXRlIEJhc2VsaW5lIHByb2ZpbGVzIGZvciBhIGdpdmVuIGNyaXRpY2FsIHVzZXIgam91cm5leS4gXCJdfSx7XCJ0YWdcIjpcImxpXCIsXCJjaGlsZHJlblwiOltcIlN1cHBvcnQgZm9yIEFsbG9jYXRpb24gbWV0cmljcyAmIHByb2ZpbGluZyBkdXJpbmcgTWljcm9iZW5jaG1hcmsgcnVucy5cIl19XX0se1widGFnXCI6XCJoclwifSx7XCJ0YWdcIjpcImFcIixcImF0dHJzXCI6e1wiaHJlZlwiOlwiaHR0cHM6Ly9naXRodWIuY29tL3kyay9jb21wb3NlX25ld3NcIn0sXCJjaGlsZHJlblwiOltcIlBvd2VyZWQgYnkgQ29tcG9zZSBOZXdzIGJvdCAoZ2l0aHViKVwiXX1dfSIsCiAgICAibWV0aG9kIjogInBvc3QiLAogICAgImhlYWRlcnMiOiB7ICJjb250ZW50LXR5cGUiOiAiYXBwbGljYXRpb24vanNvbiIgfQogIH0KfQ=="
    |> Base64.decode |> Result.get_ok
  in
  assert_and_show_diff actual expected

type bar = {headers: int; foo: string; baz: string -> unit} [@@deriving show]

let f8 () =
  let actual = show_bar {headers= 7; foo= "42"; baz= (fun _x -> ())} in
  let expected = {|{ Tests.headers = 7; foo = "42"; baz = <fun> }|} in
  check string "" expected actual

let f9 () =
  let html = {|
<html>
  <head>
    <title>FOO</title>
  </head>
</html>|} in
  let title = get_title html in
  check string "" "FOO" title

let f10 () =
  let actual = Date.parse_date "2023-07-26T00:00:00+00:00" in
  let _a = Alcotest.of_pp Date.pp in
  check (Alcotest.of_pp Date.pp) "" (Date.create 2023 7 26) actual

let () =
  run "Tests"
    [ ( "Common"
      , [ test_case "" `Quick f1
        ; test_case "" `Quick f2
        ; test_case "" `Quick f5
        ; test_case "" `Quick f6
        ; test_case "" `Quick f7
        ; test_case "" `Quick f8
        ; test_case "Extract title from html" `Quick f9
        ; test_case "" `Quick f10 ] )
    ; ("Match compose", f3)
    ; ("", f4) ]
