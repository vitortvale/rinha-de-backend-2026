open Core

type t =
  { max_amount : float
  ; max_installments : float
  ; amount_vs_avg_ratio : float
  ; max_minutes : float
  ; max_km : float
  ; max_tx_count_24h : float
  ; max_merchant_avg_amount : float
  ; mcc_risk : float String.Table.t
  }

let is_space = function
  | ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false

let skip_ws s i =
  let len = String.length s in
  let rec loop i = if i < len && is_space s.[i] then loop (i + 1) else i in
  loop i

let find_key s key =
  let pattern = "\"" ^ key ^ "\"" in
  match String.substr_index s ~pattern with
  | Some i -> i + String.length pattern
  | None -> failwith ("missing config key: " ^ key)

let value_start s key =
  let len = String.length s in
  let rec colon i = if i >= len then failwith "missing colon" else if Char.equal s.[i] ':' then i + 1 else colon (i + 1) in
  skip_ws s (colon (find_key s key))

let parse_number_at s i =
  let len = String.length s in
  let rec loop j =
    if j < len
       && (Char.is_digit s.[j]
           || Char.equal s.[j] '.'
           || Char.equal s.[j] '-'
           || Char.equal s.[j] '+'
           || Char.equal s.[j] 'e'
           || Char.equal s.[j] 'E')
    then loop (j + 1)
    else j
  in
  let j = loop i in
  Float.of_string (String.sub s ~pos:i ~len:(j - i)), j

let number s key = parse_number_at s (value_start s key) |> fst

let parse_risks s =
  let tbl = String.Table.create () in
  let len = String.length s in
  let rec loop i =
    let i = skip_ws s i in
    if i >= len then ()
    else if Char.equal s.[i] '"'
    then (
      match String.index_from s (i + 1) '"' with
      | None -> ()
      | Some key_end ->
        let key = String.sub s ~pos:(i + 1) ~len:(key_end - i - 1) in
        let value_i =
          match String.index_from s key_end ':' with
          | Some colon -> skip_ws s (colon + 1)
          | None -> failwith "bad mcc_risk entry"
        in
        let value, next = parse_number_at s value_i in
        Hashtbl.set tbl ~key ~data:value;
        loop next)
    else loop (i + 1)
  in
  loop 0;
  tbl

let load data_dir =
  let normalization = In_channel.read_all (Filename.concat data_dir "normalization.json") in
  let risks = In_channel.read_all (Filename.concat data_dir "mcc_risk.json") in
  { max_amount = number normalization "max_amount"
  ; max_installments = number normalization "max_installments"
  ; amount_vs_avg_ratio = number normalization "amount_vs_avg_ratio"
  ; max_minutes = number normalization "max_minutes"
  ; max_km = number normalization "max_km"
  ; max_tx_count_24h = number normalization "max_tx_count_24h"
  ; max_merchant_avg_amount = number normalization "max_merchant_avg_amount"
  ; mcc_risk = parse_risks risks
  }

let mcc_risk t mcc = Hashtbl.find t.mcc_risk mcc |> Option.value ~default:0.5
