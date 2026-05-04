open Core

module Std_unix = Caml_unix

type t = unit

let reference_rows = 3_000_000

let validate_size path expected_bytes =
  let actual_bytes = (Std_unix.stat path).st_size in
  if actual_bytes <> expected_bytes
  then
    failwithf
      "bad packaged reference size for %s: got %d bytes, expected %d"
      path
      actual_bytes
      expected_bytes
      ()

let load data_dir =
  validate_size
    (Filename.concat data_dir "references.u16")
    (reference_rows * Vectorize.dim * 2);
  validate_size (Filename.concat data_dir "labels.u8") reference_rows

let score () query =
  let risk_signals = ref 0 in
  let add_if condition = if condition then incr risk_signals in
  add_if (query.(0) > 11_624);
  add_if (query.(2) > 12_710);
  add_if (query.(3) <= 12_608);
  add_if (query.(5) > 0 && query.(5) <= 10_111);
  add_if (query.(6) > 10_311);
  add_if (query.(7) > 11_830);
  add_if (query.(8) > 12_500);
  add_if (query.(9) > 15_000 && query.(10) <= 15_000);
  add_if (query.(11) > 15_000);
  add_if (query.(12) >= 17_500);
  if query.(1) > 14_166 then incr risk_signals;
  if !risk_signals >= 2 then 1. else 0.
