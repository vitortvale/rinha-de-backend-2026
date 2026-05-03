FROM vitortvale/rinha-2026-ocaml-builder:latest AS build

WORKDIR /src

COPY --chown=opam:opam dune-project rinha_2026_ocaml.opam ./
COPY --chown=opam:opam src ./src
COPY --chown=opam:opam resources ./resources

RUN eval $(opam env --switch=5.2.0+ox) \
 && dune build --profile release @install \
 && gzip -cd resources/references.json.gz > /tmp/references.json \
 && _build/install/default/bin/convert_references /tmp/references.json /tmp/references.u16 /tmp/labels.u8 \
 && rm /tmp/references.json

FROM debian:12-slim AS runtime

RUN apt-get update \
 && apt-get install -y --no-install-recommends libev4 ca-certificates \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=build /src/_build/install/default/bin/rinha_api /app/rinha_api
COPY --from=build /tmp/references.u16 /app/data/references.u16
COPY --from=build /tmp/labels.u8 /app/data/labels.u8
COPY resources/mcc_risk.json /app/data/mcc_risk.json
COPY resources/normalization.json /app/data/normalization.json

ENV SOCKET_PATH=/tmp/rinha-api.sock DATA_DIR=/app/data
CMD ["/app/rinha_api"]
