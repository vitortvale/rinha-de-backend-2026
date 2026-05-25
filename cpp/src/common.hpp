#pragma once

#include <algorithm>
#include <array>
#include <atomic>
#include <cerrno>
#include <charconv>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <span>
#include <string>
#include <string_view>
#include <thread>
#include <vector>

#include <arpa/inet.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

namespace rinha {

struct fd {
  int value{-1};

  fd() = default;
  explicit fd(int raw) : value(raw) {}
  fd(const fd&) = delete;
  fd& operator=(const fd&) = delete;

  fd(fd&& other) noexcept : value(other.value) { other.value = -1; }

  fd& operator=(fd&& other) noexcept {
    if (this != &other) {
      reset();
      value = other.value;
      other.value = -1;
    }
    return *this;
  }

  ~fd() { reset(); }

  explicit operator bool() const { return value >= 0; }

  int release() {
    const auto raw = value;
    value = -1;
    return raw;
  }

  void reset(int raw = -1) {
    if (value >= 0) {
      ::close(value);
    }
    value = raw;
  }
};

inline int env_int(const char* name, int fallback) {
  const auto* raw = std::getenv(name);
  if (raw == nullptr || *raw == '\0') {
    return fallback;
  }
  int value = fallback;
  const auto text = std::string_view{raw};
  const auto result = std::from_chars(text.data(), text.data() + text.size(), value);
  return result.ec == std::errc{} ? value : fallback;
}

inline std::string env_string(const char* name, std::string fallback) {
  const auto* raw = std::getenv(name);
  return raw == nullptr || *raw == '\0' ? std::move(fallback) : std::string{raw};
}

inline std::vector<std::string> split_csv(std::string_view text) {
  std::vector<std::string> out;
  while (!text.empty()) {
    const auto comma = text.find(',');
    auto part = text.substr(0, comma);
    while (!part.empty() && part.front() == ' ') {
      part.remove_prefix(1);
    }
    while (!part.empty() && part.back() == ' ') {
      part.remove_suffix(1);
    }
    if (!part.empty()) {
      out.emplace_back(part);
    }
    if (comma == std::string_view::npos) {
      break;
    }
    text.remove_prefix(comma + 1);
  }
  return out;
}

inline void set_reuseaddr(int socket) {
  int enabled = 1;
  (void)::setsockopt(socket, SOL_SOCKET, SO_REUSEADDR, &enabled, sizeof(enabled));
}

inline void set_tcp_nodelay(int socket) {
  int enabled = 1;
  (void)::setsockopt(socket, IPPROTO_TCP, TCP_NODELAY, &enabled, sizeof(enabled));
}

inline fd tcp_listener(int port) {
  fd server{::socket(AF_INET, SOCK_STREAM, 0)};
  if (!server) {
    return {};
  }

  set_reuseaddr(server.value);

  sockaddr_in addr{};
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  addr.sin_port = htons(static_cast<std::uint16_t>(port));

  if (::bind(server.value, reinterpret_cast<sockaddr*>(&addr), sizeof(addr)) != 0) {
    return {};
  }
  if (::listen(server.value, 4096) != 0) {
    return {};
  }
  return server;
}

inline fd unix_listener(const std::string& path) {
  (void)::unlink(path.c_str());
  fd server{::socket(AF_UNIX, SOCK_STREAM, 0)};
  if (!server) {
    return {};
  }

  sockaddr_un addr{};
  addr.sun_family = AF_UNIX;
  std::strncpy(addr.sun_path, path.c_str(), sizeof(addr.sun_path) - 1);

  if (::bind(server.value, reinterpret_cast<sockaddr*>(&addr), sizeof(addr)) != 0) {
    return {};
  }
  if (::listen(server.value, 4096) != 0) {
    return {};
  }
  return server;
}

inline fd connect_unix(const std::string& path) {
  fd socket{::socket(AF_UNIX, SOCK_STREAM, 0)};
  if (!socket) {
    return {};
  }

  sockaddr_un addr{};
  addr.sun_family = AF_UNIX;
  std::strncpy(addr.sun_path, path.c_str(), sizeof(addr.sun_path) - 1);

  if (::connect(socket.value, reinterpret_cast<sockaddr*>(&addr), sizeof(addr)) != 0) {
    return {};
  }
  return socket;
}

inline bool write_all(int socket, const char* data, std::size_t size) {
  std::size_t written = 0;
  while (written < size) {
    const auto n = ::write(socket, data + written, size - written);
    if (n <= 0) {
      return false;
    }
    written += static_cast<std::size_t>(n);
  }
  return true;
}

inline bool append_read(int socket, std::vector<char>& buffer, std::size_t chunk = 8192) {
  const auto old_size = buffer.size();
  buffer.resize(old_size + chunk);
  const auto n = ::read(socket, buffer.data() + old_size, chunk);
  if (n <= 0) {
    buffer.resize(old_size);
    return false;
  }
  buffer.resize(old_size + static_cast<std::size_t>(n));
  return true;
}

}  // namespace rinha
