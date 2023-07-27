module StringMap = struct
  include Map.Make (String)

  let pp _ ppf m =
    Format.fprintf ppf "{" ;
    iter (fun k v -> Format.fprintf ppf "\"%s\": \"%s\"; " k v) m ;
    Format.fprintf ppf "}"
end

type req_props = ReqValue of string | ReqObj of (string * req_props) list
[@@deriving show]

type http_cmd_props = {url: string; props: req_props} [@@deriving show]

type http_msg_props =
  {env: string StringMap.t; headers: string StringMap.t; body: string}
[@@deriving show]

let make_telegram_request (msg : http_msg_props) (new_message : string) =
  let module J = Yojson.Safe in
  let url =
    Printf.sprintf "https://api.telegram.org/bot%s/sendMessage"
      (StringMap.find "TG_TOKEN" msg.env)
  in
  let body =
    `Assoc
      [ ("chat_id", `String (StringMap.find "CHAT_ID" msg.env))
      ; ("text", `String new_message) ]
    |> Yojson.Safe.to_string
  in
  { url
  ; props=
      ReqObj
        [ ("body", ReqValue body)
        ; ("method", ReqValue "post")
        ; ("headers", ReqObj [("content-type", ReqValue "application/json")]) ]
  }
