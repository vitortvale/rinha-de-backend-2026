#pragma once

#include "vectorize.hpp"

namespace rinha::model {

inline constexpr int unknown = -1;
inline constexpr std::int64_t threshold_low = -9000000000000000000LL;
inline constexpr std::int64_t threshold_high = 1204176005622LL;

inline std::int64_t svm_score(const vectorize::query& q) {
  std::int64_t acc = -12942553174285LL;
  acc += 180087442LL * q[0];
  acc += 276601116LL * q[1];
  acc += 452667663LL * q[2];
  acc += -140280816LL * q[3];
  acc += -14538545LL * q[4];
  acc += -14941300LL * q[5];
  acc += 112358923LL * q[6];
  acc += 323903223LL * q[7];
  acc += 241144051LL * q[8];
  acc += 10146348LL * q[9];
  acc += -7381159LL * q[10];
  acc += 131011393LL * q[11];
  acc += 262072523LL * q[12];
  acc += 145684898LL * q[13];
  acc += 4551LL * q[0] * q[0];
  acc += 6556LL * q[1] * q[1];
  acc += -9826LL * q[2] * q[2];
  acc += 8363LL * q[3] * q[3];
  acc += 312LL * q[4] * q[4];
  acc += -3070LL * q[5] * q[5];
  acc += 3069LL * q[6] * q[6];
  acc += 7423LL * q[7] * q[7];
  acc += 6941LL * q[8] * q[8];
  acc += 338LL * q[9] * q[9];
  acc += -246LL * q[10] * q[10];
  acc += 4367LL * q[11] * q[11];
  acc += 153LL * q[12] * q[12];
  acc += -34359LL * q[13] * q[13];
  acc += -4049LL * q[0] * q[1];
  acc += 5855LL * q[0] * q[2];
  acc += -12357LL * q[0] * q[3];
  acc += 2771LL * q[0] * q[4];
  acc += -385LL * q[0] * q[5];
  acc += 1213LL * q[0] * q[6];
  acc += -3879LL * q[0] * q[7];
  acc += -2051LL * q[0] * q[8];
  acc += -1371LL * q[0] * q[9];
  acc += -2234LL * q[0] * q[10];
  acc += -920LL * q[0] * q[11];
  acc += -465LL * q[0] * q[12];
  acc += -1497LL * q[0] * q[13];
  acc += -8724LL * q[1] * q[2];
  acc += -6116LL * q[1] * q[3];
  acc += 1784LL * q[1] * q[4];
  acc += -3733LL * q[1] * q[5];
  acc += 1517LL * q[1] * q[6];
  acc += -6767LL * q[1] * q[7];
  acc += -5414LL * q[1] * q[8];
  acc += 335LL * q[1] * q[9];
  acc += -876LL * q[1] * q[10];
  acc += -2439LL * q[1] * q[11];
  acc += 1421LL * q[1] * q[12];
  acc += 3964LL * q[1] * q[13];
  acc += 3110LL * q[2] * q[3];
  acc += -963LL * q[2] * q[4];
  acc += 8284LL * q[2] * q[5];
  acc += -8879LL * q[2] * q[6];
  acc += -9732LL * q[2] * q[7];
  acc += -7051LL * q[2] * q[8];
  acc += 267LL * q[2] * q[9];
  acc += 500LL * q[2] * q[10];
  acc += -2331LL * q[2] * q[11];
  acc += -1895LL * q[2] * q[12];
  acc += 18134LL * q[2] * q[13];
  acc += 1178LL * q[3] * q[4];
  acc += 4261LL * q[3] * q[5];
  acc += -3470LL * q[3] * q[6];
  acc += -4889LL * q[3] * q[7];
  acc += -4340LL * q[3] * q[8];
  acc += -169LL * q[3] * q[9];
  acc += 372LL * q[3] * q[10];
  acc += -343LL * q[3] * q[11];
  acc += -4122LL * q[3] * q[12];
  acc += 18957LL * q[3] * q[13];
  acc += -1580LL * q[4] * q[5];
  acc += 1285LL * q[4] * q[6];
  acc += 221LL * q[4] * q[7];
  acc += -534LL * q[4] * q[8];
  acc += 614LL * q[4] * q[9];
  acc += 323LL * q[4] * q[10];
  acc += 334LL * q[4] * q[11];
  acc += -1139LL * q[4] * q[12];
  acc += -5126LL * q[4] * q[13];
  acc += -3683LL * q[5] * q[6];
  acc += -957LL * q[5] * q[7];
  acc += -3545LL * q[5] * q[8];
  acc += -1300LL * q[5] * q[9];
  acc += -2035LL * q[5] * q[10];
  acc += 1151LL * q[5] * q[11];
  acc += 569LL * q[5] * q[12];
  acc += -3970LL * q[5] * q[13];
  acc += -552LL * q[6] * q[7];
  acc += 921LL * q[6] * q[8];
  acc += 2154LL * q[6] * q[9];
  acc += 2717LL * q[6] * q[10];
  acc += -1367LL * q[6] * q[11];
  acc += -1575LL * q[6] * q[12];
  acc += 4660LL * q[6] * q[13];
  acc += -3441LL * q[7] * q[8];
  acc += -1256LL * q[7] * q[9];
  acc += -1336LL * q[7] * q[10];
  acc += -4304LL * q[7] * q[11];
  acc += -198LL * q[7] * q[12];
  acc += 9358LL * q[7] * q[13];
  acc += -931LL * q[8] * q[9];
  acc += 2LL * q[8] * q[10];
  acc += -2407LL * q[8] * q[11];
  acc += 568LL * q[8] * q[12];
  acc += 645LL * q[8] * q[13];
  acc += 1346LL * q[9] * q[10];
  acc += 1LL * q[9] * q[11];
  acc += 694LL * q[9] * q[12];
  acc += -2019LL * q[9] * q[13];
  acc += 167LL * q[10] * q[11];
  acc += 546LL * q[10] * q[12];
  acc += 1931LL * q[10] * q[13];
  acc += -153LL * q[11] * q[12];
  acc += -8170LL * q[11] * q[13];
  acc += -16510LL * q[12] * q[13];
  return acc;
}

inline int decide(const vectorize::query& q) {
  const auto score = svm_score(q);
  if (score > threshold_high) {
    return 5;
  }
  if (score < threshold_low) {
    return 0;
  }
  return unknown;
}

inline int score_frauds(const vectorize::query& q) {
  return decide(q);
}

}  // namespace rinha::model
