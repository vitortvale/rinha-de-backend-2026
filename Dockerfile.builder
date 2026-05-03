FROM ocaml/opam:debian-12-ocaml-5.2

USER root
RUN apt-get update \
 && apt-get install -y --no-install-recommends zlib1g-dev libev-dev m4 pkg-config ca-certificates autoconf automake libtool make gcc git gzip \
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
