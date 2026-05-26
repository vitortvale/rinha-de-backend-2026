#include "http.hpp"
#include "index.hpp"
#include "model.hpp"

#include <csignal>
#include <cstdio>

namespace {

void respond(int client, std::string_view response) {
  (void)rinha::write_all(client, response.data(), response.size());
}

void handle_client(int raw_client, const rinha::index::ivf& index, rinha::index::scorer& scorer) {
  rinha::fd client{raw_client};
  std::vector<char> buffer;
  buffer.reserve(8192);

  rinha::http::request request{};
  while (true) {
    if (!rinha::append_read(client.value, buffer)) {
      return;
    }
    request = rinha::http::scan(std::string_view{buffer.data(), buffer.size()});
    if (request.complete) {
      break;
    }
    if (buffer.size() > 64 * 1024) {
      respond(client.value, rinha::http::bad_request_response);
      return;
    }
  }

  switch (request.target) {
    case rinha::http::route::ready:
    case rinha::http::route::warmup:
      respond(client.value, rinha::http::ready_response);
      return;
    case rinha::http::route::fraud_score: {
      const auto body = std::string_view{
          buffer.data() + request.body_start,
          request.content_length,
      };
      const auto query = rinha::vectorize::to_quantized(body);
      const auto direct = rinha::model::decide(query);
      const auto frauds = std::clamp(
          direct >= 0 ? direct : rinha::index::score_frauds(index, scorer, query),
          0,
          5);
      const auto& response = rinha::http::fraud_responses()[static_cast<std::size_t>(frauds)];
      respond(client.value, response);
      return;
    }
    case rinha::http::route::other:
      respond(client.value, rinha::http::not_found_response);
      return;
  }
}

void worker_loop(int server, const rinha::index::ivf& index) {
  auto scorer = index.create_scorer();
  while (true) {
    const int client = ::accept(server, nullptr, nullptr);
    if (client >= 0) {
      handle_client(client, index, scorer);
    }
  }
}

}  // namespace

int main() {
  std::signal(SIGPIPE, SIG_IGN);

  const auto socket_path = rinha::env_string("SOCKET_PATH", "/tmp/rinha-api.sock");
  const auto data_dir = rinha::env_string("DATA_DIR", "/app/data");
  const auto workers = std::max(1, rinha::env_int("API_WORKERS", 2));
  const auto index = rinha::index::ivf::load(data_dir);
  (void)rinha::index::prewarm(index);

  auto server = rinha::unix_listener(socket_path);
  if (!server) {
    std::perror("unix_listener");
    return 1;
  }

  std::vector<std::thread> threads;
  threads.reserve(static_cast<std::size_t>(workers));
  for (int i = 0; i < workers; ++i) {
    threads.emplace_back(worker_loop, server.value, std::cref(index));
  }
  for (auto& thread : threads) {
    thread.join();
  }
  return 0;
}
