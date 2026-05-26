# rinha-2026

The API and load balancer are being ported to C++23. The first C++ runtime slice
contains a hand-written TCP-to-Unix-socket load balancer, a Unix-socket API
server, the hot JSON quantization path, and the previous quadratic SVM gate.
Uncertain SVM rows fall back to the packaged centroid IVF index. k6/test data
remains validation-only.

Runtime shape:

- custom C++ load balancer in front
- two identical C++ API instances
- quadratic SVM hot path
- AVX2/FMA centroid IVF fallback for uncertain rows
- no generic JSON parser in the request path
