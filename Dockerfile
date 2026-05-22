ARG BUILDER_IMAGE=vitortvale/rinha-2026-ocaml-builder:latest
FROM ${BUILDER_IMAGE} AS build

WORKDIR /src

COPY --chown=opam:opam dune dune-project rinha_2026_ocaml.opam rinha_2026_ocaml_lb.opam ./
COPY --chown=opam:opam lb ./lb
COPY --chown=opam:opam src ./src
COPY --chown=opam:opam resources ./resources

FROM build AS api-build

RUN eval $(opam env --switch=5.2.0+ox) \
 && dune build --profile release @install \
 && gzip -cd resources/centroid_ivf_index.bin.gz > /tmp/centroid_ivf_index.bin \
 && _build/install/default/bin/convert_references split-ivf /tmp/centroid_ivf_index.bin /tmp/centroid_ivf_meta.bin /tmp/centroid_ivf_labels.u8 /tmp/centroid_ivf_blocks.i16

FROM build AS lb-build

RUN eval $(opam env --switch=5.2.0+ox) \
 && dune build --profile release --only-packages rinha_2026_ocaml_lb @install

FROM debian:12-slim AS runtime-deps

RUN apt-get update \
 && apt-get install -y --no-install-recommends libev4 ca-certificates \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

FROM runtime-deps AS runtime-lb

COPY --from=lb-build /src/_build/install/default/bin/rinha_lb /app/rinha_lb
ENV PORT=9999 UPSTREAMS=/sockets/api1.sock,/sockets/api2.sock LB_WORKERS=32 BUF_SIZE=8192 OCAMLRUNPARAM=s=2M,o=120
CMD ["/app/rinha_lb"]

FROM runtime-deps AS runtime-api

COPY --from=api-build /src/_build/install/default/bin/rinha_api /app/rinha_api
COPY --from=api-build /src/_build/install/default/bin/rinha_healthcheck /app/rinha_healthcheck
COPY --from=api-build /tmp/centroid_ivf_meta.bin /app/data/centroid_ivf_meta.bin
COPY --from=api-build /tmp/centroid_ivf_labels.u8 /app/data/centroid_ivf_labels.u8
COPY --from=api-build /tmp/centroid_ivf_blocks.i16 /app/data/centroid_ivf_blocks.i16
COPY resources/mcc_risk.json /app/data/mcc_risk.json
COPY resources/normalization.json /app/data/normalization.json

ENV SOCKET_PATH=/tmp/rinha-api.sock DATA_DIR=/app/data OCAMLRUNPARAM=s=2M,o=120
CMD ["/app/rinha_api"]

FROM runtime-api AS bench

COPY --from=api-build /src/_build/install/default/bin/rinha_bench /app/rinha_bench

FROM runtime-api AS runtime
