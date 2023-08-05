module StringMap = struct
  include Map.Make (String)

  let pp _ ppf m =
    Format.fprintf ppf "{" ;
    iter (Format.fprintf ppf "\"%s\": \"%s\"; ") m ;
    Format.fprintf ppf "}"
end

module Date : sig
  type t [@@deriving show]

  val create : int -> int -> int -> t

  val parse_date : string -> t

  val compare : t -> t -> int
end = struct
  type t = int * int * int [@@deriving show]

  let create year month day = (year, month, day)

  let parse_date date_str =
    Scanf.sscanf date_str "%d-%d-%dT" (fun year month day -> (year, month, day))

  let compare (y1, m1, d1) (y2, m2, d2) =
    Int.compare (y1 + 10000 + (m1 * 100) + d1) (y2 + 10000 + (m2 * 100) + d2)
end

type env =
  {tg_token: string; chat_id: string; telegraph_token: string; now: Date.t}
[@@deriving show]

let empty_env =
  {tg_token= ""; chat_id= ""; telegraph_token= ""; now= Date.create 1970 1 1}

type msg = {env: env; body: string} [@@deriving show]

module ClearText : sig
  val translate : string -> string

  val clear_text : string -> string
end = struct
  let translate = function
    | "Bug Fixes" ->
        "Исправление ошибок"
    | "API Changes" ->
        "Изменения API"
    | "Experimental K2 support" ->
        "Экспериментальная поддержка K2"
    | "Dependency Update" ->
        "Обновление зависимостей"
    | "New Features" ->
        "Новые функции"
    | text ->
        text

  let clear_html_1_re = Re.str "&#34;" |> Re.compile

  let clear_html_2_re = Re.str "&#39;" |> Re.compile

  let clear_html_3_re = Re.str "&amp;" |> Re.compile

  let clear_html_4_re = Re.str "&quot;" |> Re.compile

  let remove_issue_re = Re.Perl.compile_pat {| \([\w/, ]+\)|}

  let clear_text input =
    let decode_html text =
      text
      |> Re.replace_string clear_html_1_re ~by:"\""
      |> Re.replace_string clear_html_2_re ~by:"'"
      |> Re.replace_string clear_html_3_re ~by:"&"
      |> Re.replace_string clear_html_4_re ~by:"\""
    in
    let remove_issue input = Re.replace_string remove_issue_re input ~by:"" in
    input |> remove_issue |> decode_html
end

type req_props = ReqValue of string | ReqObj of (string * req_props) list
[@@deriving show]
