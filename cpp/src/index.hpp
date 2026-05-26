#pragma once

#include "common.hpp"
#include "vectorize.hpp"

#include <immintrin.h>

#include <cstdio>
#include <filesystem>
#include <limits>

#include <sys/mman.h>
#include <sys/stat.h>

namespace rinha::index {

inline constexpr int dim = 14;
inline constexpr int reference_rows = 3'000'000;
inline constexpr int centroid_count = 4096;
inline constexpr int neighbors = 5;

[[noreturn]] inline void fatal(const char* message) {
  std::fprintf(stderr, "%s\n", message);
  std::exit(1);
}

struct mapped_file {
  const std::byte* data{nullptr};
  std::size_t size{0};

  mapped_file() = default;
  mapped_file(const mapped_file&) = delete;
  mapped_file& operator=(const mapped_file&) = delete;

  mapped_file(mapped_file&& other) noexcept : data(other.data), size(other.size) {
    other.data = nullptr;
    other.size = 0;
  }

  mapped_file& operator=(mapped_file&& other) noexcept {
    if (this != &other) {
      reset();
      data = other.data;
      size = other.size;
      other.data = nullptr;
      other.size = 0;
    }
    return *this;
  }

  ~mapped_file() { reset(); }

  void reset() {
    if (data != nullptr) {
      ::munmap(const_cast<std::byte*>(data), size);
    }
    data = nullptr;
    size = 0;
  }

  static mapped_file open(const std::filesystem::path& path) {
    fd file{::open(path.c_str(), O_RDONLY)};
    if (!file) {
      fatal("open centroid IVF index failed");
    }

    struct stat st {};
    if (::fstat(file.value, &st) != 0 || st.st_size <= 0) {
      fatal("stat centroid IVF index failed");
    }

    void* mapped = ::mmap(nullptr, static_cast<std::size_t>(st.st_size), PROT_READ, MAP_PRIVATE, file.value, 0);
    if (mapped == MAP_FAILED) {
      fatal("mmap centroid IVF index failed");
    }

    mapped_file out;
    out.data = static_cast<const std::byte*>(mapped);
    out.size = static_cast<std::size_t>(st.st_size);
    return out;
  }
};

inline std::uint32_t u32_le(const std::byte* data, std::size_t offset) {
  const auto* p = reinterpret_cast<const unsigned char*>(data + offset);
  return static_cast<std::uint32_t>(p[0])
      | (static_cast<std::uint32_t>(p[1]) << 8)
      | (static_cast<std::uint32_t>(p[2]) << 16)
      | (static_cast<std::uint32_t>(p[3]) << 24);
}

inline int clamp_probe(int value, int fallback) {
  const int chosen = value <= 0 ? fallback : value;
  return std::clamp(chosen, 1, centroid_count);
}

struct scorer {
  std::vector<float> top_dist;
  std::vector<int> top_idx;
  std::array<double, neighbors> best_dist{};
  std::array<std::uint8_t, neighbors> best_label{};

  explicit scorer(int max_nprobe)
      : top_dist(static_cast<std::size_t>(max_nprobe), std::numeric_limits<float>::infinity()),
        top_idx(static_cast<std::size_t>(max_nprobe), 0) {}
};

struct ivf {
  mapped_file file;
  const float* centroids{nullptr};
  const std::uint32_t* offsets{nullptr};
  const std::uint8_t* labels{nullptr};
  const std::int16_t* blocks{nullptr};
  int total_blocks{0};
  int ivf_nprobe{24};
  int ivf_fast_nprobe{3};
  int ivf_verify_nprobe{24};
  int max_nprobe{24};

  static ivf load(const std::filesystem::path& data_dir) {
    ivf out;
    out.file = mapped_file::open(data_dir / "centroid_ivf_index.bin");

    if (out.file.size < 16 || static_cast<char>(out.file.data[0]) != 'I') {
      fatal("bad centroid IVF index header");
    }
    const auto rows = u32_le(out.file.data, 4);
    const auto k = u32_le(out.file.data, 8);
    const auto file_dim = u32_le(out.file.data, 12);
    if (rows != reference_rows || k != centroid_count || file_dim != dim) {
      fatal("bad centroid IVF index metadata");
    }

    const std::size_t centroids_base = 16;
    const std::size_t offsets_base = centroids_base + (static_cast<std::size_t>(dim) * centroid_count * 4);
    const std::size_t labels_base = offsets_base + ((centroid_count + 1) * 4);
    if (out.file.size < labels_base) {
      fatal("truncated centroid IVF index");
    }

    out.centroids = reinterpret_cast<const float*>(out.file.data + centroids_base);
    out.offsets = reinterpret_cast<const std::uint32_t*>(out.file.data + offsets_base);
    out.total_blocks = static_cast<int>(u32_le(out.file.data, offsets_base + (centroid_count * 4)));

    const std::size_t labels_bytes = static_cast<std::size_t>(out.total_blocks) * 8;
    const std::size_t blocks_base = labels_base + labels_bytes;
    const std::size_t blocks_bytes = static_cast<std::size_t>(out.total_blocks) * dim * 8 * sizeof(std::int16_t);
    if (blocks_base + blocks_bytes != out.file.size) {
      fatal("bad centroid IVF index size");
    }

    out.labels = reinterpret_cast<const std::uint8_t*>(out.file.data + labels_base);
    out.blocks = reinterpret_cast<const std::int16_t*>(out.file.data + blocks_base);

    out.ivf_nprobe = clamp_probe(env_int("IVF_NPROBE", 24), 24);
    out.ivf_fast_nprobe = clamp_probe(env_int("IVF_FAST_NPROBE", 3), 3);
    out.ivf_verify_nprobe = clamp_probe(env_int("IVF_VERIFY_NPROBE", out.ivf_nprobe), out.ivf_nprobe);
    out.max_nprobe = std::max({out.ivf_nprobe, out.ivf_fast_nprobe, out.ivf_verify_nprobe});
    return out;
  }

  scorer create_scorer() const { return scorer{max_nprobe}; }
};

inline void reset_best(scorer& s) {
  s.best_dist.fill(std::numeric_limits<double>::infinity());
  s.best_label.fill(0);
}

inline int frauds_of_labels(const scorer& s) {
  return static_cast<int>(s.best_label[0])
      + static_cast<int>(s.best_label[1])
      + static_cast<int>(s.best_label[2])
      + static_cast<int>(s.best_label[3])
      + static_cast<int>(s.best_label[4]);
}

inline bool should_verify_boundary(int frauds, const scorer& s) {
  return (frauds == 3 && s.best_label[4] == 1) || (frauds == 2 && s.best_label[4] == 0);
}

inline void add_candidate(scorer& s, std::uint8_t label, double dist) {
  if (dist >= s.best_dist[neighbors - 1]) {
    return;
  }
  int pos = neighbors - 1;
  while (pos > 0 && dist < s.best_dist[static_cast<std::size_t>(pos - 1)]) {
    s.best_dist[static_cast<std::size_t>(pos)] = s.best_dist[static_cast<std::size_t>(pos - 1)];
    s.best_label[static_cast<std::size_t>(pos)] = s.best_label[static_cast<std::size_t>(pos - 1)];
    --pos;
  }
  s.best_dist[static_cast<std::size_t>(pos)] = dist;
  s.best_label[static_cast<std::size_t>(pos)] = label;
}

inline void insert_top(scorer& s, int n, float dist, int centroid) {
  const auto last = static_cast<std::size_t>(n - 1);
  if (dist >= s.top_dist[last]) {
    return;
  }
  int pos = n - 1;
  while (pos > 0 && dist < s.top_dist[static_cast<std::size_t>(pos - 1)]) {
    s.top_dist[static_cast<std::size_t>(pos)] = s.top_dist[static_cast<std::size_t>(pos - 1)];
    s.top_idx[static_cast<std::size_t>(pos)] = s.top_idx[static_cast<std::size_t>(pos - 1)];
    --pos;
  }
  s.top_dist[static_cast<std::size_t>(pos)] = dist;
  s.top_idx[static_cast<std::size_t>(pos)] = centroid;
}

inline void top_centroids(const ivf& index, scorer& s, const vectorize::query& q, int n) {
  std::fill(s.top_dist.begin(), s.top_dist.begin() + n, std::numeric_limits<float>::infinity());
  std::fill(s.top_idx.begin(), s.top_idx.begin() + n, 0);

  alignas(32) float qf[dim];
  for (int d = 0; d < dim; ++d) {
    qf[d] = static_cast<float>(q[static_cast<std::size_t>(d)] - 10000) * 0.0001F;
  }

#if defined(__AVX2__)
  alignas(32) float dist_lanes[8];
  for (int centroid = 0; centroid < centroid_count; centroid += 8) {
    __m256 acc = _mm256_setzero_ps();
    for (int d = 0; d < dim; ++d) {
      const auto c = _mm256_loadu_ps(index.centroids + (static_cast<std::size_t>(d) * centroid_count) + centroid);
      const auto qv = _mm256_set1_ps(qf[d]);
      const auto delta = _mm256_sub_ps(c, qv);
#if defined(__FMA__)
      acc = _mm256_fmadd_ps(delta, delta, acc);
#else
      acc = _mm256_add_ps(acc, _mm256_mul_ps(delta, delta));
#endif
    }
    _mm256_store_ps(dist_lanes, acc);
    for (int lane = 0; lane < 8; ++lane) {
      insert_top(s, n, dist_lanes[lane], centroid + lane);
    }
  }
#else
  for (int centroid = 0; centroid < centroid_count; ++centroid) {
    float dist = 0.0F;
    for (int d = 0; d < dim; ++d) {
      const float delta = index.centroids[(static_cast<std::size_t>(d) * centroid_count) + centroid] - qf[d];
      dist += delta * delta;
    }
    insert_top(s, n, dist, centroid);
  }
#endif
}

#if defined(__AVX2__)
inline void scan_block_avx2(const ivf& index, scorer& s, int block, const std::array<int, dim>& centered) {
  const auto* block_base = index.blocks + (static_cast<std::size_t>(block) * dim * 8);
  __m256d acc_lo = _mm256_setzero_pd();
  __m256d acc_hi = _mm256_setzero_pd();

  for (int d = 0; d < dim; ++d) {
    const auto raw = _mm_loadu_si128(reinterpret_cast<const __m128i*>(block_base + (static_cast<std::size_t>(d) * 8)));
    const auto values = _mm256_cvtepi16_epi32(raw);
    const auto qv = _mm256_set1_epi32(centered[static_cast<std::size_t>(d)]);
    const auto delta = _mm256_sub_epi32(values, qv);
    const auto delta_lo = _mm256_castsi256_si128(delta);
    const auto delta_hi = _mm256_extracti128_si256(delta, 1);
    const auto x_lo = _mm256_cvtepi32_pd(delta_lo);
    const auto x_hi = _mm256_cvtepi32_pd(delta_hi);
#if defined(__FMA__)
    acc_lo = _mm256_fmadd_pd(x_lo, x_lo, acc_lo);
    acc_hi = _mm256_fmadd_pd(x_hi, x_hi, acc_hi);
#else
    acc_lo = _mm256_add_pd(acc_lo, _mm256_mul_pd(x_lo, x_lo));
    acc_hi = _mm256_add_pd(acc_hi, _mm256_mul_pd(x_hi, x_hi));
#endif
  }

  alignas(32) double dist_lo[4];
  alignas(32) double dist_hi[4];
  _mm256_store_pd(dist_lo, acc_lo);
  _mm256_store_pd(dist_hi, acc_hi);

  const auto* labels = index.labels + (static_cast<std::size_t>(block) * 8);
  add_candidate(s, labels[0], dist_lo[0]);
  add_candidate(s, labels[1], dist_lo[1]);
  add_candidate(s, labels[2], dist_lo[2]);
  add_candidate(s, labels[3], dist_lo[3]);
  add_candidate(s, labels[4], dist_hi[0]);
  add_candidate(s, labels[5], dist_hi[1]);
  add_candidate(s, labels[6], dist_hi[2]);
  add_candidate(s, labels[7], dist_hi[3]);
}
#endif

inline void scan_block_scalar(const ivf& index, scorer& s, int block, const std::array<int, dim>& centered) {
  const auto* block_base = index.blocks + (static_cast<std::size_t>(block) * dim * 8);
  const auto* labels = index.labels + (static_cast<std::size_t>(block) * 8);
  for (int lane = 0; lane < 8; ++lane) {
    double dist = 0.0;
    for (int d = 0; d < dim; ++d) {
      const int value = block_base[(static_cast<std::size_t>(d) * 8) + lane];
      const double delta = static_cast<double>(value - centered[static_cast<std::size_t>(d)]);
      dist += delta * delta;
    }
    add_candidate(s, labels[lane], dist);
  }
}

inline void scan_probe(const ivf& index, scorer& s, int centroid, const std::array<int, dim>& centered) {
  const int start_block = static_cast<int>(index.offsets[static_cast<std::size_t>(centroid)]);
  const int stop_block = static_cast<int>(index.offsets[static_cast<std::size_t>(centroid + 1)]);
  for (int block = start_block; block < stop_block; ++block) {
#if defined(__AVX2__)
    scan_block_avx2(index, s, block, centered);
#else
    scan_block_scalar(index, s, block, centered);
#endif
  }
}

inline int score_frauds(const ivf& index, scorer& s, const vectorize::query& q) {
  const int top_nprobe = std::max({index.ivf_nprobe, index.ivf_fast_nprobe, index.ivf_verify_nprobe});
  top_centroids(index, s, q, top_nprobe);

  std::array<int, dim> centered{};
  for (int d = 0; d < dim; ++d) {
    centered[static_cast<std::size_t>(d)] = q[static_cast<std::size_t>(d)] - 10000;
  }

  reset_best(s);
  for (int i = 0; i < index.ivf_fast_nprobe; ++i) {
    scan_probe(index, s, s.top_idx[static_cast<std::size_t>(i)], centered);
  }

  for (int i = index.ivf_fast_nprobe; i < index.ivf_nprobe; ++i) {
    scan_probe(index, s, s.top_idx[static_cast<std::size_t>(i)], centered);
  }

  if (index.ivf_verify_nprobe > index.ivf_nprobe) {
    for (int i = index.ivf_nprobe; i < index.ivf_verify_nprobe; ++i) {
      scan_probe(index, s, s.top_idx[static_cast<std::size_t>(i)], centered);
    }
  }
  return frauds_of_labels(s);
}

inline int prewarm(const ivf& index) {
  int checksum = 0;
  for (std::size_t offset = 0; offset < static_cast<std::size_t>(index.total_blocks) * 8; offset += 4096) {
    checksum += index.labels[offset];
  }
  for (std::size_t offset = 0; offset < static_cast<std::size_t>(index.total_blocks) * dim * 8; offset += 2048) {
    checksum += index.blocks[offset];
  }
  auto scorer = index.create_scorer();
  vectorize::query q{};
  q.fill(10000);
  checksum += score_frauds(index, scorer, q);
  return checksum;
}

}  // namespace rinha::index
