module Std_unix = Unix
open Core
open Rinha_lib

type options =
  { data_dir : string
  ; test_data : string
  ; limit : int option
  ; repeats : int
  }

let is_space = function
  | ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false
;;

let skip_ws s i =
  let len = String.length s in
  let rec loop i = if i < len && is_space s.[i] then loop (i + 1) else i in
  loop i
;;

let object_end s start =
  let len = String.length s in
  let rec loop i depth in_string escaped =
    if i >= len
    then failwith "unterminated json object"
    else (
      let c = s.[i] in
      if in_string
      then (
        let escaped' = (not escaped) && Char.equal c '\\' in
        let in_string' = if (not escaped) && Char.equal c '"' then false else true in
        loop (i + 1) depth in_string' escaped')
      else (
        match c with
        | '"' -> loop (i + 1) depth true false
        | '{' -> loop (i + 1) (depth + 1) false false
        | '}' ->
          let depth = depth - 1 in
          if depth = 0 then i else loop (i + 1) depth false false
        | _ -> loop (i + 1) depth false false))
  in
  loop start 0 false false
;;

let value_object_start s key_pos =
  let len = String.length s in
  let rec colon i =
    if i >= len
    then failwith "missing colon"
    else if Char.equal s.[i] ':'
    then i + 1
    else colon (i + 1)
  in
  let i = skip_ws s (colon key_pos) in
  if i >= len || not (Char.equal s.[i] '{') then failwith "expected request object";
  i
;;

let load_requests path limit =
  let input = In_channel.read_all path in
  let marker = "\"request\"" in
  let rec loop offset acc =
    if Option.value_map limit ~default:false ~f:(fun limit -> List.length acc >= limit)
    then List.rev acc |> Array.of_list
    else (
      match String.substr_index input ~pos:offset ~pattern:marker with
      | None -> List.rev acc |> Array.of_list
      | Some marker_pos ->
        let start = value_object_start input (marker_pos + String.length marker) in
        let stop = object_end input start in
        let request = String.sub input ~pos:start ~len:(stop - start + 1) in
        loop (stop + 1) (request :: acc))
  in
  loop 0 []
;;

let allocated_words before after =
  after.Gc.Stat.minor_words
  +. after.major_words
  -. (before.Gc.Stat.minor_words +. before.major_words)
;;

let run_case name ~repeats requests f =
  Gc.compact ();
  let before = Gc.quick_stat () in
  let started = Std_unix.gettimeofday () in
  let checksum = ref 0. in
  for _ = 1 to repeats do
    Array.iter requests ~f:(fun request ->
      checksum := !checksum +. Sys.opaque_identity (f request))
  done;
  let elapsed = Std_unix.gettimeofday () -. started in
  let after = Gc.quick_stat () in
  let ops = Float.of_int (Array.length requests * repeats) in
  let ns_per_op = elapsed *. 1_000_000_000. /. ops in
  let words_per_op = allocated_words before after /. ops in
  printf
    "%-18s %10.2f ns/op %10.2f words/op checksum %.1f\n%!"
    name
    ns_per_op
    words_per_op
    !checksum
;;

let check_ivf_matches_vp index queries =
  let centroid_ivf_mismatches = ref 0 in
  let centroid_ivf_decision_mismatches = ref 0 in
  Array.iteri queries ~f:(fun i query ->
    let vp = Index.score_frauds_vp index query in
    let centroid_ivf = Index.score_frauds_centroid_ivf index query in
    if centroid_ivf <> vp then incr centroid_ivf_mismatches;
    if not (Bool.equal (centroid_ivf >= 3) (vp >= 3))
    then incr centroid_ivf_decision_mismatches;
    if !centroid_ivf_decision_mismatches <= 10
       && not (Bool.equal (centroid_ivf >= 3) (vp >= 3))
    then
      printf
        "centroid_ivf_decision_mismatch query=%d vp=%d centroid_ivf=%d\n%!"
        i
        vp
        centroid_ivf);
  printf
    "centroid_ivf_mismatches=%d centroid_ivf_decision_mismatches=%d/%d\n%!"
    !centroid_ivf_mismatches
    !centroid_ivf_decision_mismatches
    (Array.length queries);
  if !centroid_ivf_decision_mismatches = 0
  then printf "ivf_decision_correctness=ok queries=%d\n%!" (Array.length queries)
;;

let main { data_dir; test_data; limit; repeats } =
  printf "data_dir=%s test_data=%s repeats=%d\n%!" data_dir test_data repeats;
  let requests = load_requests test_data limit in
  printf "requests=%d\n%!" (Array.length requests);
  let config = Config.load data_dir in
  let index = Index.load_for_bench data_dir in
  let scorer = Index.create_scorer index in
  let queries = Array.map requests ~f:(Vectorize.to_quantized config) in
  check_ivf_matches_vp index queries;
  run_case "parse" ~repeats requests (fun request ->
    let tx = Runtime_json.parse request in
    tx.amount);
  run_case "quantize" ~repeats requests (fun request ->
    let query = Vectorize.to_quantized config request in
    Float.of_int query.(0));
  let query = Array.create ~len:Vectorize.dim 0 in
  run_case "quantize_into" ~repeats requests (fun request ->
    Vectorize.to_quantized_into config request query;
    Float.of_int query.(0));
  run_case "score_vp" ~repeats queries (fun query ->
    Float.of_int (Index.score_frauds_vp index query) /. Float.of_int Index.k);
  run_case "score_centroid_ivf" ~repeats queries (fun query ->
    Float.of_int (Index.score_frauds_with_scorer index scorer query)
    /. Float.of_int Index.k);
  run_case "score" ~repeats queries (fun query ->
    Float.of_int (Index.score_frauds_with_scorer index scorer query)
    /. Float.of_int Index.k);
  run_case "full" ~repeats requests (fun request ->
    let query = Vectorize.to_quantized config request in
    Float.of_int (Index.score_frauds_with_scorer index scorer query)
    /. Float.of_int Index.k);
  run_case "full_into" ~repeats requests (fun request ->
    Vectorize.to_quantized_into config request query;
    Float.of_int (Index.score_frauds_with_scorer index scorer query)
    /. Float.of_int Index.k)
;;

let usage program =
  eprintf
    "usage: %s [-data-dir DIR] [-test-data PATH] [-limit N] [-repeats N]\n%!"
    program;
  exit 2
;;

let parse_args argv =
  let data_dir = ref "resources" in
  let test_data = ref "/tmp/rinha-2026-specs/test/test-data.json" in
  let limit = ref None in
  let repeats = ref 1 in
  let rec loop = function
    | [] -> ()
    | "-data-dir" :: value :: rest ->
      data_dir := value;
      loop rest
    | "-test-data" :: value :: rest ->
      test_data := value;
      loop rest
    | "-limit" :: value :: rest ->
      limit := Some (Int.of_string value);
      loop rest
    | "-repeats" :: value :: rest ->
      repeats := Int.of_string value;
      loop rest
    | _ -> usage argv.(0)
  in
  loop (Array.to_list argv |> List.tl_exn);
  { data_dir = !data_dir; test_data = !test_data; limit = !limit; repeats = !repeats }
;;

let () = main (parse_args (Sys.get_argv ()))
