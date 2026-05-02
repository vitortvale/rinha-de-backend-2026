FROM ocaml/opam:debian-12-ocaml-5.2 AS build

USER root
RUN apt-get update \
 && apt-get install -y --no-install-recommends python3 zlib1g-dev libev-dev m4 pkg-config ca-certificates autoconf automake libtool make gcc git \
 && rm -rf /var/lib/apt/lists/*

USER opam
WORKDIR /src

RUN opam repository add ox git+https://github.com/oxcaml/opam-repository.git \
 && opam switch create 5.2.0+ox --repos ox=git+https://github.com/oxcaml/opam-repository.git,default --yes

RUN eval $(opam env --switch=5.2.0+ox) \
 && OPAMSOLVERTIMEOUT=300 opam install --unlock-base --yes \
      oxcaml-compiler.5.2.0minus31 \
      ocaml-variants.5.2.0+ox \
      dune.3.20.2+ox1 \
      core.v0.18~preview.130.91+190 \
      async.v0.18~preview.130.91+190

COPY --chown=opam:opam dune-project rinha_2026_ocaml.opam ./
COPY --chown=opam:opam src ./src
COPY --chown=opam:opam tools ./tools
COPY --chown=opam:opam resources ./resources

RUN eval $(opam env --switch=5.2.0+ox) \
 && dune build --profile release @install \
 && python3 tools/convert_references.py resources/references.json.gz /tmp/references.u16 /tmp/labels.u8

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
