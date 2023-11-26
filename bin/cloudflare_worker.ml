open Js_of_ocaml
open Js_of_ocaml.Js

module Response = struct
  type t = Response

  let text (r : t) : string Promise.t = Unsafe.meth_call r "text" [||]

  let status (r : t) : int = Unsafe.get r "status"
end

module Fetch = struct
  type request_init = {todo: int}

  let fetch (url : string) props : Response.t Promise.t =
    let _body =
      props
      |> List.map (fun prop ->
             match prop with
             | `Method (name : string) ->
                 ("method", Unsafe.inject name)
             | `Body (body : string) ->
                 ("body", Js.string body |> Unsafe.inject)
             | `Headers headers ->
                 ( "headers"
                 , headers
                   |> List.map (fun (k, (v : string)) -> (k, Unsafe.inject v))
                   |> Array.of_list |> Unsafe.obj ) )
      |> Array.of_list |> Unsafe.obj
    in
    Unsafe.global##fetch (Unsafe.inject url) _body
end

module CloudflareWorker = struct
  let scheduled (handle_scheduled : unit -> unit Promise.t) =
    Dom.addEventListener Unsafe.global
      (Dom.Event.make "scheduled")
      (Dom.handler (fun e -> e##waitUntil (handle_scheduled ())))
      _false
    |> ignore
end
