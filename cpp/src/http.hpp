#pragma once

#include "common.hpp"

namespace rinha::http {

enum class route { ready, warmup, fraud_score, other };

struct request {
  route target{route::other};
  std::size_t content_length{0};
  std::size_t body_start{0};
  std::size_t total_size{0};
  bool complete{false};
};

inline bool starts_with(std::string_view text, std::string_view prefix) {
  return text.size() >= prefix.size() && text.substr(0, prefix.size()) == prefix;
}

inline route parse_route(std::string_view line) {
  if (starts_with(line, "POST /fraud-score ")) {
    return route::fraud_score;
  }
  if (starts_with(line, "GET /warmup ")) {
    return route::warmup;
  }
  if (starts_with(line, "GET /ready ")) {
    return route::ready;
  }
  return route::other;
}

inline char lower_ascii(char c) {
  return c >= 'A' && c <= 'Z' ? static_cast<char>(c + 32) : c;
}

inline bool header_name_eq(std::string_view line, std::string_view name) {
  if (line.size() < name.size()) {
    return false;
  }
  for (std::size_t i = 0; i < name.size(); ++i) {
    if (lower_ascii(line[i]) != name[i]) {
      return false;
    }
  }
  return true;
}

inline std::size_t parse_content_length(std::string_view line) {
  auto pos = line.find(':');
  if (pos == std::string_view::npos) {
    return 0;
  }
  ++pos;
  while (pos < line.size() && (line[pos] == ' ' || line[pos] == '\t')) {
    ++pos;
  }
  std::size_t value = 0;
  const auto result = std::from_chars(line.data() + pos, line.data() + line.size(), value);
  return result.ec == std::errc{} ? value : 0;
}

inline request scan(std::string_view data) {
  request out{};
  const auto line_end = data.find("\r\n");
  if (line_end == std::string_view::npos) {
    return out;
  }
  out.target = parse_route(data.substr(0, line_end));

  std::size_t pos = line_end + 2;
  while (pos + 1 < data.size()) {
    if (data[pos] == '\r' && data[pos + 1] == '\n') {
      out.body_start = pos + 2;
      out.total_size = out.body_start + out.content_length;
      out.complete = data.size() >= out.total_size;
      return out;
    }
    const auto next = data.find("\r\n", pos);
    if (next == std::string_view::npos) {
      return out;
    }
    const auto line = data.substr(pos, next - pos);
    if (header_name_eq(line, "content-length:")) {
      out.content_length = parse_content_length(line);
    }
    pos = next + 2;
  }
  return out;
}

inline constexpr std::string_view ready_response =
    "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nOK";

inline constexpr std::string_view bad_request_response =
    "HTTP/1.1 400 Bad Request\r\nContent-Length: 11\r\nConnection: close\r\n\r\nbad request";

inline constexpr std::string_view not_found_response =
    "HTTP/1.1 404 Not Found\r\nContent-Length: 9\r\nConnection: close\r\n\r\nnot found";

inline const std::array<std::string, 6>& fraud_responses() {
  static const auto responses = [] {
    std::array<std::string, 6> out{};
    for (int frauds = 0; frauds <= 5; ++frauds) {
      const bool approved = frauds < 3;
      const char* score = nullptr;
      switch (frauds) {
        case 0: score = "0.0"; break;
        case 1: score = "0.2"; break;
        case 2: score = "0.4"; break;
        case 3: score = "0.6"; break;
        case 4: score = "0.8"; break;
        default: score = "1.0"; break;
      }
      const std::string body = std::string{"{\"approved\":"}
          + (approved ? "true" : "false")
          + ",\"fraud_score\":"
          + score
          + "}";
      out[static_cast<std::size_t>(frauds)] =
          "HTTP/1.1 200 OK\r\nContent-Length: "
          + std::to_string(body.size())
          + "\r\nConnection: close\r\n\r\n"
          + body;
    }
    return out;
  }();
  return responses;
}

}  // namespace rinha::http
