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
