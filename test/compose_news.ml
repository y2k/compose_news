let read_sample_file filename =
  let ch = open_in_bin ("../../../test/samples/" ^ filename) in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

open Lib

let () =
  (* RSS *)
  read_sample_file "rss.xml" |> Update_loader.main
  |> List.map Update_loader.show_item
  |> List.fold_left ( ^ ) "\n" |> print_endline ;
  print_endline "\n--------------------------------------" ;
  (* HTML *)
  read_sample_file "sample.html"
  |> Html_parser.parse "2.6.0-alpha08"
  |> List.map Html_parser.show_item
  |> List.fold_left ( ^ ) "\n" |> print_endline
