module StringMap = Map.Make (String)

type content = {title: string; link: string} [@@deriving show]

type item = {date: string; links: content list} [@@deriving show]

let get_links xml =
  xml |> Xml.parse_string |> Xml.children
  |> List.concat_map Xml.children
  |> List.map (fun x ->
         { title= Xml.children x |> List.hd |> Xml.pcdata
         ; link= Xml.attrib x "href" } )

let main () =
  let xml = Xml.parse_file "rss.xml" in
  Xml.children xml
  |> List.filter (fun x -> Xml.tag x = "entry")
  |> List.map (fun x ->
         x
         |> Xml.fold
              (fun a x ->
                match (Xml.tag x, Xml.children x) with
                | "title", [ch] ->
                    {a with date= Xml.pcdata ch}
                | "content", [ch] ->
                    {a with links= get_links (Xml.pcdata ch)}
                | _ ->
                    a )
              {date= ""; links= []} )
  |> List.iter (fun x -> print_endline (show_item x)) ;
  flush_all ()
