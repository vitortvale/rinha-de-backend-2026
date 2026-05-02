# Rinha 2026 OCaml/OxCaml

Fraud detection API for Rinha de Backend 2026 using OCaml/OxCaml and Jane Street Async.

The service exposes the required endpoints:

- `GET /ready`
- `POST /fraud-score`

The Docker topology follows the contest shape: HAProxy on port `9999`, round-robin to two API instances over Unix domain sockets.

## Local build

```sh
docker compose build
docker compose up
```

## Prebuilt image compose

For a Docker Hub image, push a linux/amd64 image and run compose with the image override:

```sh
docker buildx build --platform linux/amd64 -t your-dockerhub-user/rinha-2026-ocaml:latest --push .
API_IMAGE=your-dockerhub-user/rinha-2026-ocaml:latest docker compose -f docker-compose.image.yml up
```

`docker-compose.image.yml` has no `build:` entries, so it pulls the pushed image instead.

The runtime does not use a generic JSON library. It uses a purpose-built parser for the fixed contest payload schema, and the image build converts `resources/references.json.gz` into packed binary files:

- `references.u16`: 14 dimensions per row, with values quantized as `(value + 1.0) * 10000`
- `labels.u8`: `1` for fraud, `0` for legit

This avoids boxed OCaml float storage for the 3M-reference dataset.
