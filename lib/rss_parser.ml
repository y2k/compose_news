type content = {title: string; link: string; version: string}
[@@deriving show] [@@deriving yojson]

type entry = {updated: string; links: content list} [@@deriving show]

let get_links xml =
  xml |> Xml.parse_string |> Xml.children
  |> List.concat_map Xml.children
  |> List.map (fun x ->
         { title= Xml.children x |> List.hd |> Xml.pcdata
         ; link= Xml.attrib x "href"
         ; version=
             (let url = Xml.attrib x "href" in
              let start = String.index url '#' + 1 in
              String.sub url start (String.length url - start) ) } )

let main xml_string =
  xml_string |> Xml.parse_string |> Xml.children
  |> List.filter (fun x -> Xml.tag x = "entry")
  |> List.map (fun x ->
         x
         |> Xml.fold
              (fun a x ->
                match (Xml.tag x, Xml.children x) with
                | "updated", [ch] ->
                    {a with updated= Xml.pcdata ch}
                | "content", [ch] ->
                    {a with links= get_links (Xml.pcdata ch)}
                | _ ->
                    a )
              {updated= ""; links= []} )
