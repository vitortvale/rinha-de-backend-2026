#pragma once

#include "common.hpp"

namespace rinha::vectorize {

using query = std::array<int, 14>;

inline double clamp01(double value) {
  return value < 0.0 ? 0.0 : (value > 1.0 ? 1.0 : value);
}

inline int quantize_clamped(double x) {
  const auto y = (x + 1.0) * 10000.0;
  if (y <= 0.0) {
    return 0;
  }
  if (y >= 65535.0) {
    return 65535;
  }
  return static_cast<int>(y + 0.5);
}

inline int digit(char c) { return static_cast<int>(c - '0'); }
inline int int2(std::string_view s, std::size_t pos) { return digit(s[pos]) * 10 + digit(s[pos + 1]); }
inline int int4(std::string_view s, std::size_t pos) { return int2(s, pos) * 100 + int2(s, pos + 2); }

inline int days_from_civil(int year, int month, int day) {
  year -= month <= 2 ? 1 : 0;
  const int era = (year >= 0 ? year : year - 399) / 400;
  const unsigned yoe = static_cast<unsigned>(year - era * 400);
  const unsigned doy = (153 * static_cast<unsigned>(month + (month > 2 ? -3 : 9)) + 2) / 5
      + static_cast<unsigned>(day) - 1;
  const unsigned doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
  return era * 146097 + static_cast<int>(doe) - 719468;
}

struct timestamp_parts {
  int hour{};
  int day_of_week{};
  int epoch_minutes{};
};

inline timestamp_parts timestamp_at(std::string_view s, std::size_t pos) {
  const int year = int4(s, pos);
  const int month = int2(s, pos + 5);
  const int day = int2(s, pos + 8);
  const int hour = int2(s, pos + 11);
  const int minute = int2(s, pos + 14);
  const int epoch_days = days_from_civil(year, month, day);
  return {
      .hour = hour,
      .day_of_week = (epoch_days + 3) % 7,
      .epoch_minutes = ((epoch_days * 24 + hour) * 60) + minute,
  };
}

inline bool is_ws(char c) { return c == ' ' || c == '\n' || c == '\r' || c == '\t'; }

inline std::size_t skip_ws(std::string_view s, std::size_t pos) {
  while (pos < s.size() && is_ws(s[pos])) {
    ++pos;
  }
  return pos;
}

inline std::size_t next_value(std::string_view s, std::size_t pos) {
  while (pos < s.size() && s[pos] != ':') {
    ++pos;
  }
  return skip_ws(s, pos + (pos < s.size() ? 1 : 0));
}

inline std::size_t string_end(std::string_view s, std::size_t pos) {
  bool escaped = false;
  for (; pos < s.size(); ++pos) {
    const char c = s[pos];
    if (c == '"' && !escaped) {
      return pos;
    }
    escaped = c == '\\' && !escaped;
    if (c != '\\') {
      escaped = false;
    }
  }
  return s.size();
}

inline std::size_t skip_string_value(std::string_view s, std::size_t pos) {
  return pos < s.size() && s[pos] == '"' ? string_end(s, pos + 1) + 1 : pos;
}

inline std::pair<std::size_t, std::size_t> string_bounds_at(std::string_view s, std::size_t pos) {
  const auto start = pos + 1;
  const auto stop = string_end(s, start);
  return {start, stop - start};
}

inline std::size_t skip_array_value(std::string_view s, std::size_t pos) {
  if (pos >= s.size() || s[pos] != '[') {
    return pos;
  }
  for (++pos; pos < s.size(); ++pos) {
    if (s[pos] == ']') {
      return pos + 1;
    }
    if (s[pos] == '"') {
      pos = string_end(s, pos + 1);
    }
  }
  return s.size();
}

inline double parse_number_value_at(std::string_view s, std::size_t pos) {
  bool negative = false;
  if (pos < s.size() && (s[pos] == '-' || s[pos] == '+')) {
    negative = s[pos] == '-';
    ++pos;
  }

  double value = 0.0;
  while (pos < s.size() && s[pos] >= '0' && s[pos] <= '9') {
    value = value * 10.0 + static_cast<double>(digit(s[pos++]));
  }
  if (pos < s.size() && s[pos] == '.') {
    double factor = 0.1;
    for (++pos; pos < s.size() && s[pos] >= '0' && s[pos] <= '9'; ++pos) {
      value += static_cast<double>(digit(s[pos])) * factor;
      factor *= 0.1;
    }
  }
  if (pos < s.size() && (s[pos] == 'e' || s[pos] == 'E')) {
    ++pos;
    bool exp_negative = false;
    if (pos < s.size() && (s[pos] == '-' || s[pos] == '+')) {
      exp_negative = s[pos] == '-';
      ++pos;
    }
    int exponent = 0;
    while (pos < s.size() && s[pos] >= '0' && s[pos] <= '9') {
      exponent = exponent * 10 + digit(s[pos++]);
    }
    double scale = 1.0;
    for (int i = 0; i < exponent; ++i) {
      scale *= 10.0;
    }
    value = exp_negative ? value / scale : value * scale;
  }
  return negative ? -value : value;
}

inline int parse_int_at(std::string_view s, std::size_t pos) {
  int sign = 1;
  if (pos < s.size() && (s[pos] == '-' || s[pos] == '+')) {
    sign = s[pos] == '-' ? -1 : 1;
    ++pos;
  }
  int value = 0;
  while (pos < s.size() && s[pos] >= '0' && s[pos] <= '9') {
    value = value * 10 + digit(s[pos++]);
  }
  return value * sign;
}

inline bool bool_at(std::string_view s, std::size_t pos) {
  return pos < s.size() && s[pos] == 't';
}

inline bool is_null_at(std::string_view s, std::size_t pos) {
  return pos + 4 <= s.size() && s.substr(pos, 4) == "null";
}

inline int mcc_code_at(std::string_view s, std::size_t pos) {
  if (pos >= s.size() || s[pos] != '"' || pos + 5 >= s.size()) {
    return -1;
  }
  int value = 0;
  for (std::size_t i = 1; i <= 4; ++i) {
    if (s[pos + i] < '0' || s[pos + i] > '9') {
      return -1;
    }
    value = value * 10 + digit(s[pos + i]);
  }
  return value;
}

inline double mcc_risk_code(int mcc) {
  switch (mcc) {
    case 5411: return 0.15;
    case 5812: return 0.30;
    case 5912: return 0.20;
    case 5944: return 0.45;
    case 7801: return 0.80;
    case 7802: return 0.75;
    case 7995: return 0.85;
    case 4511: return 0.35;
    case 5311: return 0.25;
    case 5999: return 0.50;
    default: return 0.50;
  }
}

inline bool array_contains_string_slice_at(
    std::string_view s,
    std::size_t pos,
    std::size_t needle_start,
    std::size_t needle_len) {
  if (pos >= s.size() || s[pos] != '[') {
    return false;
  }
  for (++pos; pos < s.size(); ++pos) {
    if (s[pos] == ']') {
      return false;
    }
    if (s[pos] != '"') {
      continue;
    }
    const auto [start, len] = string_bounds_at(s, pos);
    if (len == needle_len && s.substr(start, len) == s.substr(needle_start, needle_len)) {
      return true;
    }
    pos = start + len;
  }
  return false;
}

inline timestamp_parts timestamp_parts_value_at(std::string_view s, std::size_t pos) {
  return timestamp_at(s, pos + 1);
}

inline query to_quantized(std::string_view body) {
  query q{};

  const auto id_start = next_value(body, 0);
  const auto transaction_start = next_value(body, skip_string_value(body, id_start));
  const auto amount_start = next_value(body, transaction_start);
  const auto installments_start = next_value(body, amount_start);
  const auto requested_at_start = next_value(body, installments_start);
  const double amount = parse_number_value_at(body, amount_start);
  const int installments = parse_int_at(body, installments_start);
  const auto requested_at = timestamp_parts_value_at(body, requested_at_start);

  const auto customer_start = next_value(body, skip_string_value(body, requested_at_start));
  const auto customer_avg_amount_start = next_value(body, customer_start);
  const auto tx_count_24h_start = next_value(body, customer_avg_amount_start);
  const auto known_merchants_start = next_value(body, tx_count_24h_start);
  const double customer_avg_amount = parse_number_value_at(body, customer_avg_amount_start);
  const int tx_count_24h = parse_int_at(body, tx_count_24h_start);

  const auto merchant_start = next_value(body, skip_array_value(body, known_merchants_start));
  const auto merchant_id_value_start = next_value(body, merchant_start);
  const auto merchant_mcc_start = next_value(body, skip_string_value(body, merchant_id_value_start));
  const auto merchant_avg_amount_start = next_value(body, skip_string_value(body, merchant_mcc_start));
  const auto [merchant_id_start, merchant_id_len] = string_bounds_at(body, merchant_id_value_start);
  const int merchant_mcc = mcc_code_at(body, merchant_mcc_start);
  const double merchant_avg_amount = parse_number_value_at(body, merchant_avg_amount_start);

  const auto terminal_start = next_value(body, merchant_avg_amount_start);
  const auto is_online_start = next_value(body, terminal_start);
  const auto card_present_start = next_value(body, is_online_start);
  const auto km_from_home_start = next_value(body, card_present_start);
  const bool is_online = bool_at(body, is_online_start);
  const bool card_present = bool_at(body, card_present_start);
  const double km_from_home = parse_number_value_at(body, km_from_home_start);
  const bool known_merchant =
      array_contains_string_slice_at(body, known_merchants_start, merchant_id_start, merchant_id_len);

  double minutes_since_last = -1.0;
  double km_from_last = -1.0;
  const auto last_start = next_value(body, km_from_home_start);
  if (!is_null_at(body, last_start)) {
    const auto timestamp_start = next_value(body, last_start);
    const auto km_from_current_start = next_value(body, skip_string_value(body, timestamp_start));
    const auto last_at = timestamp_parts_value_at(body, timestamp_start);
    minutes_since_last =
        clamp01(static_cast<double>(requested_at.epoch_minutes - last_at.epoch_minutes) / 1440.0);
    km_from_last = clamp01(parse_number_value_at(body, km_from_current_start) / 1000.0);
  }

  q[0] = quantize_clamped(clamp01(amount / 10000.0));
  q[1] = quantize_clamped(clamp01(static_cast<double>(installments) / 12.0));
  q[2] = quantize_clamped(clamp01((amount / std::max(customer_avg_amount, 0.000001)) / 10.0));
  q[3] = quantize_clamped(static_cast<double>(requested_at.hour) / 23.0);
  q[4] = quantize_clamped(static_cast<double>(requested_at.day_of_week) / 6.0);
  q[5] = quantize_clamped(minutes_since_last);
  q[6] = quantize_clamped(km_from_last);
  q[7] = quantize_clamped(clamp01(km_from_home / 1000.0));
  q[8] = quantize_clamped(clamp01(static_cast<double>(tx_count_24h) / 20.0));
  q[9] = quantize_clamped(is_online ? 1.0 : 0.0);
  q[10] = quantize_clamped(card_present ? 1.0 : 0.0);
  q[11] = quantize_clamped(known_merchant ? 0.0 : 1.0);
  q[12] = quantize_clamped(mcc_risk_code(merchant_mcc));
  q[13] = quantize_clamped(clamp01(merchant_avg_amount / 10000.0));
  return q;
}

}  // namespace rinha::vectorize
