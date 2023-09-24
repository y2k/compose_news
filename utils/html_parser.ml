open Angstrom

(*
<div><h3 id="1.2.0-beta01" data-text="Version 1.2.0-beta01">Version 1.2.0-beta01</h3>
    <p>July 18, 2023</p>
    <p><code translate="no" dir="ltr">androidx.benchmark:benchmark-*:1.2.0-beta01</code> is released.
      <a href="https://android.googlesource.com/platform/frameworks/support/+log/3b5b931546a48163444a9eddc533489fcddd7494..c2c1745b1d05b772c7bbe82834a9197ba5b85114/benchmark">Version
        1.2.0-beta01 contains these commits.</a></p>
    <p><strong>Bug Fixes</strong></p>
    <ul>
        <li>Fix warnings being sometimes suppressed in Benchmark output in Studio, and workaround leading whitespaces
            from Benchmark output not showing up in Studio (<a
                    href="https://android-review.googlesource.com/#/q/Ia61d0fdc52ca0de2ece777ea6dd76bc77876fcf2">Ia61d0</a>,
            <a href="https://issuetracker.google.com/issues/227205461">b/227205461</a>, <a
                    href="https://issuetracker.google.com/issues/286306579">b/286306579</a>, <a
                    href="https://issuetracker.google.com/issues/285912360">b/285912360</a>)
        </li>
        <li>Fixed comment for <code translate="no" dir="ltr">FrameTimingMetric</code>. The submetric is named <code
                translate="no" dir="ltr">frameDurationCpuMs</code>. (<a
                href="https://android-review.googlesource.com/#/q/Ib097f4f633ede0f2932aaaff7c9d5dbbb74daf88">Ib097f</a>,
            <a href="https://issuetracker.google.com/issues/288830934">b/288830934</a>).
        </li>
    </ul>
</div>
*)

type dom =
  | Text of string
  | Element of string * (string * string) list * dom list
[@@deriving show]

let r2 = Re.str "<h3" |> Re.compile

let get_new_substring prefix html =
  let html = html |> String.map (fun x -> if x = '\n' then ' ' else x) in
  let r1 = Re.str ({|<h3 id="|} ^ prefix ^ "\"") |> Re.compile in
  let start_pos = Re.Group.start (Re.exec r1 html) 0 in
  let end_pos = Re.Group.start (Re.exec ~pos:(start_pos + 1) r2 html) 0 in
  String.sub html start_pos (end_pos - start_pos)
  |> Printf.sprintf "<div>%s</div>"

let pspaces = skip_while (fun c -> c = ' ')

let is_lex c =
  (c >= '0' && c <= '9')
  || (c >= 'A' && c <= 'Z')
  || (c >= 'a' && c <= 'z')
  || c = '_' || c = '-'

let is_value c = is_lex c || c = '.'

let pattr_key = take_while1 is_lex

let pattr_val = take_till (fun c -> c = '"')

let pattr_kv =
  lift2 (fun a b -> (a, b)) pattr_key (string "=\"" *> pattr_val <* char '"')

let pattr_kvs = many (pspaces *> pattr_kv)

let ptag_name = take_while1 is_lex

(*  *)

let pstart_tag =
  lift2 (fun a b -> (a, b)) (char '<' *> ptag_name) (pattr_kvs <* char '>')

let ptext = take_while1 (fun c -> c <> '<' && c <> '>') >>| fun s -> Text s

let pend_tag = string "</" *> take_while1 is_lex <* char '>' >>| ignore

(*  *)

type item = {title: string; content: string list} [@@deriving show]

module Items = struct
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

  let rec dom_to_items (xs : dom list) =
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
        :: dom_to_items xs
    | _ :: xs ->
        dom_to_items xs
    | _ ->
        []
end

(*  *)

let presult : dom t =
  fix (fun (presult : dom t) ->
      lift2
        (fun (a, b) xs -> Element (a, b, xs))
        pstart_tag
        (many (choice [ptext; presult]))
      <* pend_tag )

let parse_partial expr str = parse_string ~consume:All expr str

let parse prefix html =
  let html = html |> get_new_substring prefix in
  match parse_partial presult html with
  | Ok (Element (_, _, xs)) ->
      Items.dom_to_items xs
  | Ok x ->
      failwith @@ "Wrong DOM: " ^ show_dom x
  | Error msg ->
      print_endline html ; failwith msg
