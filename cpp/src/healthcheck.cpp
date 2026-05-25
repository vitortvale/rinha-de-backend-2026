#include "common.hpp"

#include <cstdio>

int main() {
  const auto socket_path = rinha::env_string("SOCKET_PATH", "/tmp/rinha-api.sock");
  auto socket = rinha::connect_unix(socket_path);
  if (!socket) {
    std::perror("connect_unix");
    return 1;
  }

  constexpr std::string_view request =
      "GET /warmup HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n";
  if (!rinha::write_all(socket.value, request.data(), request.size())) {
    return 1;
  }

  std::array<char, 128> buffer{};
  const auto n = ::read(socket.value, buffer.data(), buffer.size());
  if (n <= 0) {
    return 1;
  }
  const auto response = std::string_view{buffer.data(), static_cast<std::size_t>(n)};
  return response.starts_with("HTTP/1.1 200") ? 0 : 1;
}
