ARG BUILDER_IMAGE=vitortvale/rinha-2026-ocaml-builder:latest
FROM ${BUILDER_IMAGE} AS build

WORKDIR /src

COPY --chown=opam:opam dune dune-project rinha_2026_ocaml.opam ./
COPY --chown=opam:opam src ./src
COPY --chown=opam:opam resources ./resources

RUN eval $(opam env --switch=5.2.0+ox) \
 && dune build --profile release @install \
 && gzip -cd resources/centroid_ivf_index.bin.gz > /tmp/centroid_ivf_index.bin \
 && _build/install/default/bin/convert_references split-ivf /tmp/centroid_ivf_index.bin /tmp/centroid_ivf_meta.bin /tmp/centroid_ivf_labels.u8 /tmp/centroid_ivf_blocks.i16

FROM debian:12-slim AS runtime-base

RUN apt-get update \
 && apt-get install -y --no-install-recommends libev4 ca-certificates \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=build /src/_build/install/default/bin/rinha_api /app/rinha_api
COPY --from=build /src/_build/install/default/bin/rinha_healthcheck /app/rinha_healthcheck
COPY --from=build /tmp/centroid_ivf_meta.bin /app/data/centroid_ivf_meta.bin
COPY --from=build /tmp/centroid_ivf_labels.u8 /app/data/centroid_ivf_labels.u8
COPY --from=build /tmp/centroid_ivf_blocks.i16 /app/data/centroid_ivf_blocks.i16
COPY resources/mcc_risk.json /app/data/mcc_risk.json
COPY resources/normalization.json /app/data/normalization.json

ENV SOCKET_PATH=/tmp/rinha-api.sock DATA_DIR=/app/data OCAMLRUNPARAM=s=2M,o=120

FROM runtime-base AS bench

COPY --from=build /src/_build/install/default/bin/rinha_bench /app/rinha_bench

FROM runtime-base AS runtime

CMD ["/app/rinha_api"]
