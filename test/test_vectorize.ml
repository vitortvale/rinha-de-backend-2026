open Core
open Rinha_lib

let approx a b = Float.(abs (a -. b) < 0.0002)

let () =
  let config = Config.load "../resources" in
  let tx =
    Runtime_json.parse
      {|{"id":"tx-1329056812","transaction":{"amount":41.12,"installments":2,"requested_at":"2026-03-11T18:45:53Z"},"customer":{"avg_amount":82.24,"tx_count_24h":3,"known_merchants":["MERC-003","MERC-016"]},"merchant":{"id":"MERC-016","mcc":"5411","avg_amount":60.25},"terminal":{"is_online":false,"card_present":true,"km_from_home":29.23},"last_transaction":null}|}
  in
  let v = Vectorize.to_float_array config tx in
  assert (Array.length v = 14);
  assert (approx v.(0) 0.0041);
  assert (approx v.(1) 0.1667);
  assert (approx v.(2) 0.05);
  assert (approx v.(3) 0.7826);
  assert (approx v.(4) 0.3333);
  assert (Float.equal v.(5) (-1.));
  assert (Float.equal v.(6) (-1.));
  assert (approx v.(7) 0.0292);
  assert (approx v.(8) 0.15);
  assert (Float.equal v.(9) 0.);
  assert (Float.equal v.(10) 1.);
  assert (Float.equal v.(11) 0.);
  assert (approx v.(12) 0.15);
  assert (approx v.(13) 0.006)
