(*************)
(* influxdb line protocol reporter *)
(* from https://docs.influxdata.com/influxdb/v1.5/write_protocols/line_protocol_reference/ *)
(* example line: weather,location=us-midwest temperature=82 1465839830100400200 *)
(*************)

open Astring

let avoid_keyword =
  let keywords = String.Set.of_list [
    "ALL" ; "ALTER" ; "ANY" ; "AS" ; "ASC" ; "BEGIN" ;
    "BY" ; "CREATE" ; "CONTINUOUS" ; "DATABASE" ; "DATABASES" ; "DEFAULT" ;
    "DELETE" ; "DESC" ; "DESTINATIONS" ; "DIAGNOSTICS" ; "DISTINCT" ; "DROP" ;
    "DURATION" ; "END" ; "EVERY" ; "EXPLAIN" ; "FIELD" ; "FOR" ;
    "FROM" ; "GRANT" ; "GRANTS" ; "GROUP" ; "GROUPS" ; "IN" ;
    "INF" ; "INSERT" ; "INTO" ; "KEY" ; "KEYS" ; "KILL" ;
    "LIMIT" ; "SHOW" ; "MEASUREMENT" ; "MEASUREMENTS" ; "NAME" ; "OFFSET" ;
    "ON" ; "ORDER" ; "PASSWORD" ; "POLICY" ; "POLICIES" ; "PRIVILEGES" ;
    "QUERIES" ; "QUERY" ; "READ" ; "REPLICATION" ; "RESAMPLE" ; "RETENTION" ;
    "REVOKE" ; "SELECT" ; "SERIES" ; "SET" ; "SHARD" ; "SHARDS" ;
    "SLIMIT" ; "SOFFSET" ; "STATS" ; "SUBSCRIPTION" ; "SUBSCRIPTIONS" ; "TAG" ;
    "TO" ; "USER" ; "USERS" ; "VALUES" ; "WHERE" ; "WITH" ; "WRITE"
  ] in
  (fun m -> if String.(Set.mem (Ascii.uppercase m) keywords) then "o" ^ m else m)

let escape =
  List.fold_right (fun e m' -> String.(concat ~sep:("\\" ^ e) (cuts ~sep:e m')))

let escape_measurement m = escape  [ "," ; " " ] (avoid_keyword m)

let escape_name m = escape [ "," ; " " ; "=" ] (avoid_keyword m)

let pp_value (str : string Fmt.t) ppf f =
  let open Metrics in
  match value f with
  | V (String, s) -> str ppf s
  | V (Int, i) -> Fmt.pf ppf "%di" i
  | V (Int32, i32) -> Fmt.pf ppf "%ldi" i32
  | V (Int64, i64) -> Fmt.pf ppf "%Ldi" i64
  | V (Uint, u) -> Fmt.pf ppf "%ui" u
  | V (Uint32, u32) -> Fmt.pf ppf "%lui" u32
  | V (Uint64, u64) -> Fmt.pf ppf "%Lui" u64
  | _ -> pp_value ppf f

(* we need to:
  - avoid keywords
  - escape comma and space in measurement name
  - escape comma, space and equal in tag key, tag value, field key of type string
  - double-quote field value of type string
  - data type number is a float, suffix i for integers *)
let encode_line_protocol tags data name =
  let data_fields = Metrics.Data.fields data in
  let pp_field_str ppf s = Fmt.pf ppf "%S" s in
  let pp_field ppf f =
    Fmt.(pair ~sep:(unit "=") string (pp_value pp_field_str)) ppf
      (escape_name (Metrics.key f), f)
  in
  let pp_fields = Fmt.(list ~sep:(unit ",") pp_field) in
  let pp_tag_str ppf s = Fmt.string ppf (escape_name s) in
  let pp_tag ppf f =
    Fmt.(pair ~sep:(unit "=") string (pp_value pp_tag_str)) ppf
      (escape_name (Metrics.key f), f)
  in
  let pp_tags = Fmt.(list ~sep:(unit ",") pp_tag) in
  Fmt.strf "%s,%a %a" (escape_measurement name) pp_tags tags pp_fields data_fields

let influxdb_reporter () =
  let report ~tags ~data ~over src k =
    let str = encode_line_protocol tags data (Metrics.Src.name src) in
    Fmt.pr "%s\n%!" str ;
    over ();
    k ()
  in
  let now () = Mtime_clock.now () |> Mtime.to_uint64_ns in
  { Metrics.report; now ; at_exit = (fun () -> ()) }

(*************)
(*   Tests   *)
(*************)

let tags = Metrics.Tags.[ string "hostname" ]

let i = Metrics.gc_quick_stat ~tags
let i2 = Metrics.gc_stat ~tags

let timer =
  let open Metrics in
  let tags = Tags.[string "function"] in
  let data () = Data.v [] in
  Src.fn "duration" ~tags ~data ~duration:true ~status:false

let t_local t = t "local"

let f s tags =
  Metrics.add s tags (fun m -> m ())

let f () =
  Metrics.run timer (fun t -> t "stat") (fun m -> m ()) (fun () -> f i2 t_local) ;
  Metrics.run timer (fun t -> t "quick_stat") (fun m -> m ()) (fun () -> f i t_local)

let mem = Hashtbl.create 2

let alloc n =
  match Hashtbl.find_opt mem n with
  | Some v -> v
  | None ->
    let r =
      let rec alloc = function
        | 0 -> ["hallo"]
        | n -> string_of_int n :: alloc (pred n)
      in
      List.init n alloc
    in
    Hashtbl.add mem n r ;
    r

let run () =
  for i = 0 to 1000 do
    f () ;
    let _ = alloc i in
    Unix.sleep 1
  done

let () =
  Metrics.enable_all () ;
  Metrics.set_reporter (influxdb_reporter ()) ;
  run ();
