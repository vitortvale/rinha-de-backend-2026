# rinha-2026

The API and load balancer are being ported to C++23. The first C++ runtime slice
contains a hand-written TCP-to-Unix-socket load balancer, a Unix-socket API
server, the hot JSON quantization path, and the previous quadratic SVM gate.

The IVF fallback has not been ported yet. Until that lands, the C++ API uses a
temporary cheap fallback rule after the SVM gate. k6/test data remains
validation-only.

Runtime shape:

- custom C++ load balancer in front
- two identical C++ API instances
- quadratic SVM hot path
- temporary rule fallback for uncertain rows
- no generic JSON parser in the request path
