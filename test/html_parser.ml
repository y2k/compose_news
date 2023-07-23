(*

<h3 id="2.7.0-beta02" data-text="Version 2.7.0-beta02">Version 2.7.0-beta02</h3>

<p>June 28, 2023</p>

<p><code translate="no" dir="ltr">androidx.navigation:navigation-*:2.7.0-beta02</code> is released. <a href="https://android.googlesource.com/platform/frameworks/support/+log/6e6b38595c10b8edd5d40a9bc9d4e294fc4f8160/navigation">Version 2.7.0-beta02 contains these commits.</a></p>

<p><strong>Bug Fixes</strong></p>

<ul>
<li>Navigation Compose now has the right z-order for custom transitions that use navigate with the <code translate="no" dir="ltr">popUpTo</code> option.(<a href="https://android-review.googlesource.com/#/q/Ib1c3a329755b2cbec3d28568d03d73e5aa9b9128">/Ib1c3a</a>, <a href="https://issuetracker.google.com/285153947">b/285153947</a>)</li>
</ul>

<h3 id="2.7.0-beta01" data-text="Version 2.7.0-beta01">Version 2.7.0-beta01</h3>
*)

let get_new_substring prefix html =
  let html = html |> String.map (fun x -> if x = '\n' then ' ' else x) in
  let regex = Str.regexp ({|<h3 id="|} ^ prefix ^ {|".*?<p|}) in
  Str.search_forward regex html 0 |> ignore ;
  let regex = Str.regexp {|<h3.+|} in
  let prev_begin = Str.match_beginning () + 1 in
  Str.search_forward regex html prev_begin |> ignore ;
  String.sub html (prev_begin - 1) (Str.match_beginning () - prev_begin)
  |> ( ^ ) "<div>"

type dom =
  | Text of string
  | Element of string * (string * string) list * dom list
[@@deriving show]

let rec dom_items_to_string (xs : dom list) =
  List.fold_left
    (fun a x ->
      let s =
        match x with
        | Text x ->
            x
        | Element (_, _, xs) ->
            dom_items_to_string xs
      in
      a ^ s )
    "" xs

type item = {title: string; content: string list} [@@deriving show]

let rec parse (xs : dom list) =
  match xs with
  | Element ("p", _, [Element ("strong", _, [Text title])])
    :: _
    :: Element ("ul", _, _ :: items)
    :: xs ->
      { title
      ; content=
          items
          |> List.filter_map (function
               | Text _ ->
                   None
               | Element (_, _, xs) ->
                   Some (dom_items_to_string xs) ) }
      :: parse xs
  | _ :: xs ->
      parse xs
  | _ ->
      []

let parse prefix html =
  let html = html |> get_new_substring prefix in
  (* print_endline html ; *)
  html |> Markup.string |> Markup.parse_html |> Markup.signals
  |> Markup.tree
       ~text:(fun ss -> Text (String.concat "" ss))
       ~element:(fun (_, name) xs children ->
         let attrs = xs |> List.map (fun ((_, name), value) -> (name, value)) in
         Element (name, attrs, children) )
  (* |> Option.map (fun x ->
         print_endline (show_dom x) ;
         x ) *)
  |> Option.map (function Element (_, _, xs) -> parse xs | _ -> failwith "???")
  |> Option.map (List.map show_item)
  |> Option.fold ~none:[] ~some:Fun.id
  |> List.fold_left ( ^ ) "\n"
