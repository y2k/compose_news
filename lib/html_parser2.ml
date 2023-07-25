open MParser

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

let tag_name =
  many1 (choice [alphanum; char '-'])
  |>> fun xs -> xs |> List.to_seq |> String.of_seq

let tag_start_left = char '<' >> tag_name

let key = tag_name

let value_inside =
  many1 (choice [alphanum; space])
  |>> fun xs -> xs |> List.to_seq |> String.of_seq

let value = between (char '"') (char '"') value_inside

let attrs =
  many
    (spaces1 >> (key << char '=') >>= fun name -> value |>> fun v -> (name, v))

let tag_start =
  between (char '<') (char '>')
    (tag_name >>= fun tname -> attrs |>> fun attrs2 -> (tname, attrs2))
  |>> fun (name, attrs) -> Element (name, attrs, [])

let tag_end = between (char '<' >> char '/') (char '>') tag_name

let complete_tag = tag_start << tag_end

(*
<div></div>
<div foo="bar"></div>
*)

let parse text =
  parse_string complete_tag text ()
  |> (function Success e -> e | Failed (msg, _e) -> failwith msg)
  |> show_dom
