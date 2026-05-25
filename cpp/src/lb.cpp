#include "http.hpp"

#include <csignal>
#include <cstdio>

namespace {

struct config {
  int port{9999};
  int workers{32};
  std::size_t buffer_size{8192};
  std::vector<std::string> upstreams;
};

config load_config() {
  auto upstreams = rinha::split_csv(
      rinha::env_string("UPSTREAMS", "/sockets/api1.sock,/sockets/api2.sock"));
  if (upstreams.empty()) {
    upstreams.emplace_back("/sockets/api1.sock");
  }
  return {
      .port = rinha::env_int("PORT", 9999),
      .workers = std::max(1, rinha::env_int("LB_WORKERS", 32)),
      .buffer_size = static_cast<std::size_t>(std::max(4096, rinha::env_int("BUF_SIZE", 8192))),
      .upstreams = std::move(upstreams),
  };
}

bool read_request(int client, std::vector<char>& buffer, rinha::http::request& request, std::size_t chunk) {
  buffer.clear();
  while (true) {
    if (!rinha::append_read(client, buffer, chunk)) {
      return false;
    }
    request = rinha::http::scan(std::string_view{buffer.data(), buffer.size()});
    if (request.complete) {
      return true;
    }
    if (buffer.size() > 128 * 1024) {
      return false;
    }
  }
}

void proxy_one(
    int raw_client,
    const config& cfg,
    std::atomic<std::uint64_t>& next_upstream) {
  rinha::fd client{raw_client};
  rinha::set_tcp_nodelay(client.value);

  std::vector<char> request_buffer;
  request_buffer.reserve(cfg.buffer_size);
  rinha::http::request request{};
  if (!read_request(client.value, request_buffer, request, cfg.buffer_size)) {
    return;
  }

  const auto index = next_upstream.fetch_add(1, std::memory_order_relaxed) % cfg.upstreams.size();
  auto upstream = rinha::connect_unix(cfg.upstreams[index]);
  if (!upstream) {
    return;
  }

  if (!rinha::write_all(upstream.value, request_buffer.data(), request.total_size)) {
    return;
  }

  std::vector<char> response;
  response.reserve(cfg.buffer_size);
  while (rinha::append_read(upstream.value, response, cfg.buffer_size)) {
  }
  if (!response.empty()) {
    (void)rinha::write_all(client.value, response.data(), response.size());
  }
}

void worker_loop(int server, const config& cfg, std::atomic<std::uint64_t>& next_upstream) {
  while (true) {
    const int client = ::accept(server, nullptr, nullptr);
    if (client >= 0) {
      proxy_one(client, cfg, next_upstream);
    }
  }
}

}  // namespace

int main() {
  std::signal(SIGPIPE, SIG_IGN);

  const auto cfg = load_config();
  auto server = rinha::tcp_listener(cfg.port);
  if (!server) {
    std::perror("tcp_listener");
    return 1;
  }

  std::atomic<std::uint64_t> next_upstream{0};
  std::vector<std::thread> threads;
  threads.reserve(static_cast<std::size_t>(cfg.workers));
  for (int i = 0; i < cfg.workers; ++i) {
    threads.emplace_back(worker_loop, server.value, std::cref(cfg), std::ref(next_upstream));
  }
  for (auto& thread : threads) {
    thread.join();
  }
  return 0;
}
