let read_sample_file filename =
  let ch = open_in_bin ("../../../test/samples/" ^ filename) in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

open Lib

let _main () =
  (* RSS *)
  read_sample_file "rss.xml" |> Update_loader.main
  |> List.map Update_loader.show_item
  |> List.fold_left ( ^ ) "\n" |> print_endline ;
  print_endline "\n--------------------------------------" ;
  (* HTML *)
  read_sample_file "sample2.html"
  |> Html_parser.parse "1.2.0-beta01"
  |> List.map Html_parser.show_item
  |> List.fold_left ( ^ ) "\n" |> print_endline

let () =
  Html_parser2.parse "<div></div>" |> print_endline ;
  Html_parser2.parse {|<div foo="bar"></div>|} |> print_endline;
  Html_parser2.parse {|<div foo="bar" data-text="Version"></div>|} |> print_endline;
  Html_parser2.parse {|<div foo="bar" data-text="Version">xxx</div>|} |> print_endline;
