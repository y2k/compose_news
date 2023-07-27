open Js_of_ocaml
module U = Js.Unsafe
open Lib.Core

let execute_request (url : string) props =
  let rec mk_req = function
    | ReqObj props ->
        U.obj (Array.of_list (List.map (fun (k, p) -> (k, mk_req p)) props))
    | ReqValue v ->
        U.inject v
  in
  U.global##fetch (U.inject url) (mk_req props)

let execute_request_ (p : http_cmd_props) =
  let rec mk_req = function
    | ReqObj props ->
        U.obj (Array.of_list (List.map (fun (k, p) -> (k, mk_req p)) props))
    | ReqValue v ->
        Js.string v |> U.inject
  in
  U.global##fetch (U.inject p.url) (mk_req p.props)

let entries_to_string_map entries =
  U.global ##. Array##from entries
  |> Js.to_array
  |> Array.fold_left
       (fun a e ->
         let k = e##at 0 in
         if Js.typeof k = Js.string "string" then
           StringMap.add (Js.to_string k) (Js.to_string (e##at 1)) a
         else a )
       StringMap.empty

let make_response (body : string) =
  U.new_obj U.global ##. Response [|U.inject body|]

let get_entries obj = U.global ##. Object##entries obj

let log_error e = U.global##.console##error e

let fetch__ (url : string) =
  let promise = execute_request url (ReqObj []) in
  (promise##catch (fun e -> log_error e))##then_ (fun result -> result##text)

let fetch_ (url : string) (callback : _ -> unit) =
  let promise = execute_request url (ReqObj []) in
  let result_promise =
    (promise##catch (fun e -> log_error e))##then_ (fun result -> result##text)
  in
  callback result_promise

let make_env () : string StringMap.t =
  List.to_seq
    [("TG_TOKEN", U.global ##. TG_TOKEN_); ("CHAT_ID", U.global ##. CHAT_ID_)]
  |> StringMap.of_seq

let fetch (* (handle : http_msg_props -> http_cmd_props option) *) req env =
  print_endline "LOG :: CALLED" ;
  req##text##then_ (fun body ->
      match
        (fun _ -> None)
          {env= entries_to_string_map (get_entries env); body= Js.to_string body}
      with
      | Some cmd ->
          let promise = execute_request cmd.url cmd.props in
          (promise##catch (fun e -> log_error e))##then_ (fun _ ->
              make_response "" )
      | None ->
          make_response "test - response" )
