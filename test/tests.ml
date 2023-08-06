open Lib
open Lib.Utils
module Date = Utils.Date

let _ = Test_effects.main ()

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

let assert_and_show_diff actual expected =
  if actual <> expected then (
    TextComparer.compare_2_txt expected actual ;
    prerr_endline @@ "========================\n"
    ^ Base64.encode_string actual
    ^ "\n========================" ;
    failwith "" |> ignore )

(* let get_actual (file_name : string) version : string =
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
     [{title= "title"; link= "link"; version}]
     [ { env=
           { tg_token= "TG_TOKEN"
           ; chat_id= "CHAT_ID"
           ; telegraph_token= "e43a8cbce190"
           ; now= Date.create 2023 1 1 }
       ; body= read_sample_file file_name } ]
   |> fun f ->
   f Command.World ignore ;
   !logs |> List.fold_left ( ^ ) "" *)

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
           ; telegraph_token= "e43a8cbce190"
           ; now= Date.create 2023 7 day }
       ; body= read_sample_file file_name } )
  |> fun f ->
  f Command.World ignore ;
  !logs |> List.fold_left ( ^ ) ""

let () =
  let assert_date day expected : unit =
    let actual = get_actual_2 "rss2.xml" day in
    assert_and_show_diff actual (expected |> Base64.decode |> Result.get_ok)
  in
  assert_date 25 "" ;
  assert_date 26
    "ewogICJ1cmwiOiAiaHR0cHM6Ly9kZXZlbG9wZXIuYW5kcm9pZC5jb20vamV0cGFjay9hbmRyb2lkeC9yZWxlYXNlcy93ZWFyLWNvbXBvc2UjMS4yLjAtcmMwMSIsCiAgInByb3BzIjoge30KfXsKICAidXJsIjogImh0dHBzOi8vZGV2ZWxvcGVyLmFuZHJvaWQuY29tL2pldHBhY2svYW5kcm9pZHgvcmVsZWFzZXMvd2Vhci1jb21wb3NlIzEuMC4wLWFscGhhMDgiLAogICJwcm9wcyI6IHt9Cn17CiAgInVybCI6ICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL3dlYXItY29tcG9zZSMxLjMuMC1hbHBoYTAyIiwKICAicHJvcHMiOiB7fQp9ewogICJ1cmwiOiAiaHR0cHM6Ly9kZXZlbG9wZXIuYW5kcm9pZC5jb20vamV0cGFjay9hbmRyb2lkeC9yZWxlYXNlcy9jb25zdHJhaW50bGF5b3V0IzEuMS4wLWFscGhhMTEiLAogICJwcm9wcyI6IHt9Cn17CiAgInVybCI6ICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL2NvbXBvc2UtdWkjMS42LjAtYWxwaGEwMiIsCiAgInByb3BzIjoge30KfXsKICAidXJsIjogImh0dHBzOi8vZGV2ZWxvcGVyLmFuZHJvaWQuY29tL2pldHBhY2svYW5kcm9pZHgvcmVsZWFzZXMvY29tcG9zZS11aSMxLjUuMC1yYzAxIiwKICAicHJvcHMiOiB7fQp9ewogICJ1cmwiOiAiaHR0cHM6Ly9kZXZlbG9wZXIuYW5kcm9pZC5jb20vamV0cGFjay9hbmRyb2lkeC9yZWxlYXNlcy9jb21wb3NlLXJ1bnRpbWUjMS42LjAtYWxwaGEwMiIsCiAgInByb3BzIjoge30KfXsKICAidXJsIjogImh0dHBzOi8vZGV2ZWxvcGVyLmFuZHJvaWQuY29tL2pldHBhY2svYW5kcm9pZHgvcmVsZWFzZXMvY29tcG9zZS1ydW50aW1lIzEuNS4wLXJjMDEiLAogICJwcm9wcyI6IHt9Cn17CiAgInVybCI6ICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL2NvbXBvc2UtbWF0ZXJpYWwzIzEuMi4wLWFscGhhMDQiLAogICJwcm9wcyI6IHt9Cn17CiAgInVybCI6ICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL2NvbXBvc2UtbWF0ZXJpYWwjMS42LjAtYWxwaGEwMiIsCiAgInByb3BzIjoge30KfXsKICAidXJsIjogImh0dHBzOi8vZGV2ZWxvcGVyLmFuZHJvaWQuY29tL2pldHBhY2svYW5kcm9pZHgvcmVsZWFzZXMvY29tcG9zZS1tYXRlcmlhbCMxLjUuMC1yYzAxIiwKICAicHJvcHMiOiB7fQp9ewogICJ1cmwiOiAiaHR0cHM6Ly9kZXZlbG9wZXIuYW5kcm9pZC5jb20vamV0cGFjay9hbmRyb2lkeC9yZWxlYXNlcy9jb21wb3NlLWZvdW5kYXRpb24jMS42LjAtYWxwaGEwMiIsCiAgInByb3BzIjoge30KfXsKICAidXJsIjogImh0dHBzOi8vZGV2ZWxvcGVyLmFuZHJvaWQuY29tL2pldHBhY2svYW5kcm9pZHgvcmVsZWFzZXMvY29tcG9zZS1mb3VuZGF0aW9uIzEuNS4wLXJjMDEiLAogICJwcm9wcyI6IHt9Cn17CiAgInVybCI6ICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL2NvbXBvc2UtY29tcGlsZXIjMS41LjEiLAogICJwcm9wcyI6IHt9Cn17CiAgInVybCI6ICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL2NvbXBvc2UtYW5pbWF0aW9uIzEuNi4wLWFscGhhMDIiLAogICJwcm9wcyI6IHt9Cn17CiAgInVybCI6ICJodHRwczovL2RldmVsb3Blci5hbmRyb2lkLmNvbS9qZXRwYWNrL2FuZHJvaWR4L3JlbGVhc2VzL2NvbXBvc2UtYW5pbWF0aW9uIzEuNS4wLXJjMDEiLAogICJwcm9wcyI6IHt9Cn0=" ;
  assert_date 27 ""

(* let () =
     let actual = get_actual "sample3.html" "1.5.0-rc01" in
     let expected =
       "ewogICJ1cmwiOiAiaHR0cHM6Ly9hcGkudGVsZWdyYW0ub3JnL2JvdFRHX1RPS0VOL3NlbmRNZXNzYWdlIiwKICAicHJvcHMiOiB7CiAgICAiYm9keSI6ICJ7XCJjaGF0X2lkXCI6XCJDSEFUX0lEXCIsXCJ0ZXh0XCI6XCJcXG50aXRsZVxcblxcblsg0JjRgdC/0YDQsNCy0LvQtdC90LjQtSDQvtGI0LjQsdC+0LogXVxcbi0gRml4ZWQgYW4gaXNzdWUgd2hlcmUgY2FsbGluZyAudmFsdWUgb24gYSBwcmltaXRpdmUgc3RhdGUgdHlwZSB3b3VsZCByZXBvcnQgYSBsaW50IHdhcm5pbmcgd2l0aCBhbiBpbnZhbGlkIGZpeC4gVGhlIGluc3BlY3Rpb24gd2lsbCBub3cgcmVjb21tZW5kIG1pZ3JhdGluZyB0byB0aGUgY29ycmVjdCBwcm9wZXJ0eS5cXG4tIEFuIG9wdGlvbmFsIGluc3BlY3Rpb24gdG8gcmVjb21tZW5kIG1pZ3JhdGluZyBtdXRhYmxlU3RhdGVPZigpIGNhbGxzIHRvIHRoZWlyIGNvcnJlc3BvbmRpbmcgc3BlY2lhbGl6ZWQgdHlwZXMgZm9yIHByaW1pdGl2ZXMgaXMgYXZhaWxhYmxlLiBJdHMgbGludCBJRCBpcyBBdXRvYm94aW5nU3RhdGVDcmVhdGlvbi4gUHJldmlvdXNseSwgdGhpcyBpbnNwZWN0aW9uIHdhcyBlbmFibGVkIGJ5IGRlZmF1bHQgZm9yIGFsbCBwcm9qZWN0cy4gVG8gc2VlIHRoaXMgd2FybmluZyBpbiBBbmRyb2lkIFN0dWRpbydzIGVkaXRvciBhbmQgeW91ciBwcm9qZWN0J3MgbGludCBvdXRwdXRzLCBjaGFuZ2UgaXRzIHNldmVyaXR5IGZyb20gaW5mb3JtYXRpb25hbCB0byB3YXJuaW5nIGJ5IGRlY2xhcmluZyB3YXJuaW5nIFxcXCJBdXRvYm94aW5nU3RhdGVDcmVhdGlvblxcXCIgaW5zaWRlIHlvdXIgbW9kdWxlJ3MgYnVpbGQuZ3JhZGxlIG9yIGJ1aWxkLmdyYWRsZS5rdHMgY29uZmlndXJhdGlvbiBhcyBzaG93bjogICAgIGFuZHJvaWQgeyAgICAgICAgIGxpbnQgeyAgICAgICAgICAgICB3YXJuaW5nIFxcXCJBdXRvYm94aW5nU3RhdGVDcmVhdGlvblxcXCIgICAgICAgICB9ICAgICAgICAgLi4uICAgICB9IFwifSIsCiAgICAibWV0aG9kIjogInBvc3QiLAogICAgImhlYWRlcnMiOiB7ICJjb250ZW50LXR5cGUiOiAiYXBwbGljYXRpb24vanNvbiIgfQogIH0KfQ=="
       |> Base64.decode |> Result.get_ok
     in
     assert_and_show_diff actual expected

   let () =
     let actual = get_actual "sample2.html" "1.1.0" in
     let expected =
       "ewogICJ1cmwiOiAiaHR0cHM6Ly9hcGkudGVsZWdyYW0ub3JnL2JvdFRHX1RPS0VOL3NlbmRNZXNzYWdlIiwKICAicHJvcHMiOiB7CiAgICAiYm9keSI6ICJ7XCJjaGF0X2lkXCI6XCJDSEFUX0lEXCIsXCJ0ZXh0XCI6XCJcXG50aXRsZVxcblxcblsgSW1wb3J0YW50IGNoYW5nZXMgc2luY2UgMS4wLjAgXVxcbi0gU3VwcG9ydCBmb3IgSmV0cGFjayBNYWNyb2JlbmNobWFya3MsIHdoaWNoIGFsbG93cyB5b3UgdG8gbWVhc3VyZSB3aG9sZS1hcHAgaW50ZXJhY3Rpb25zIGxpa2Ugc3RhcnR1cCBhbmQgc2Nyb2xsaW5nLCBwcm92aWRlcyB0aGUgYWJpbGl0eSB0byBjYXB0dXJlIHRyYWNlcyAgJiBtZWFzdXJlIHRyYWNlIHNlY3Rpb25zLlxcbi0gU3VwcG9ydCBmb3IgQmFzZWxpbmUgUHJvZmlsZXMgICBDb21waWxhdGlvbk1vZGUuUGFydGlhbCB0byBtZWFzdXJlIHRoZSBlZmZlY3RpdmVuZXNzIG9mIEJhc2VsaW5lIFByb2ZpbGVzLiBAQmFzZWxpbmVQcm9maWxlUnVsZSB0byBhdXRvbWF0aWNhbGx5IGdlbmVyYXRlIEJhc2VsaW5lIHByb2ZpbGVzIGZvciBhIGdpdmVuIGNyaXRpY2FsIHVzZXIgam91cm5leS4gXFxuLSBTdXBwb3J0IGZvciBBbGxvY2F0aW9uIG1ldHJpY3MgJiBwcm9maWxpbmcgZHVyaW5nIE1pY3JvYmVuY2htYXJrIHJ1bnMuXCJ9IiwKICAgICJtZXRob2QiOiAicG9zdCIsCiAgICAiaGVhZGVycyI6IHsgImNvbnRlbnQtdHlwZSI6ICJhcHBsaWNhdGlvbi9qc29uIiB9CiAgfQp9"
       |> Base64.decode |> Result.get_ok
     in
     assert_and_show_diff actual expected *)

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
