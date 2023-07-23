let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

let () =
  read_whole_file "../../../test/sample.html"
  |> Html_parser.parse "2.6.0-alpha08"
  |> print_endline ;
  Update_loader.main ()
