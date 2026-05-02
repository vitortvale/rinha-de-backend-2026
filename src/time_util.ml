open Core

let int2 s pos =
  ((Char.to_int s.[pos] - Char.to_int '0') * 10)
  + (Char.to_int s.[pos + 1] - Char.to_int '0')

let int4 s pos = (int2 s pos * 100) + int2 s (pos + 2)

let days_from_civil year month day =
  let year = if month <= 2 then year - 1 else year in
  let era = if year >= 0 then year / 400 else (year - 399) / 400 in
  let yoe = year - (era * 400) in
  let month_prime = month + if month > 2 then -3 else 9 in
  let doy = ((153 * month_prime) + 2) / 5 + day - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

let parts timestamp =
  let year = int4 timestamp 0 in
  let month = int2 timestamp 5 in
  let day = int2 timestamp 8 in
  let hour = int2 timestamp 11 in
  let minute = int2 timestamp 14 in
  year, month, day, hour, minute

let hour timestamp =
  let _, _, _, hour, _ = parts timestamp in
  hour

let day_of_week timestamp =
  let year, month, day, _, _ = parts timestamp in
  let days = days_from_civil year month day in
  Int.( % ) (days + 3) 7

let epoch_minutes timestamp =
  let year, month, day, hour, minute = parts timestamp in
  (((days_from_civil year month day * 24) + hour) * 60) + minute
