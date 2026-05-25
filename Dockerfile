FROM debian:12-slim AS build

RUN apt-get update \
 && apt-get install -y --no-install-recommends build-essential cmake ninja-build ca-certificates \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /src

COPY CMakeLists.txt ./
COPY cpp ./cpp

RUN cmake -S . -B /build -G Ninja -DCMAKE_BUILD_TYPE=Release \
 && cmake --build /build

FROM debian:12-slim AS runtime-deps

RUN apt-get update \
 && apt-get install -y --no-install-recommends ca-certificates \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

FROM runtime-deps AS runtime-lb

COPY --from=build /build/rinha_lb /app/rinha_lb
ENV PORT=9999 UPSTREAMS=/sockets/api1.sock,/sockets/api2.sock LB_WORKERS=32 BUF_SIZE=8192
CMD ["/app/rinha_lb"]

FROM runtime-deps AS runtime-api

COPY --from=build /build/rinha_api /app/rinha_api
COPY --from=build /build/rinha_healthcheck /app/rinha_healthcheck
COPY resources/mcc_risk.json /app/data/mcc_risk.json
COPY resources/normalization.json /app/data/normalization.json

ENV SOCKET_PATH=/tmp/rinha-api.sock DATA_DIR=/app/data
CMD ["/app/rinha_api"]

FROM runtime-api AS bench

FROM runtime-api AS runtime
