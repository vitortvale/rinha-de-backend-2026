# Agent Instructions

This repository has a hard submission rule for the current performance track:

- The `submission` branch Docker Compose topology must use direct single-active TCP.
- The only services allowed in `docker-compose.yml` on `submission` are `api1` and `api2`.
- `api1` must run the immutable API image tag, publish host port `9999`, and set `TCP_PORT=9999`.
- `api1` must receive the active resources: `cpus: "0.999"` and `memory: "344MB"`.
- `api2` must use the same immutable API image tag but run `command: [ "sleep", "infinity" ]`.
- `api2` must keep the second instance slot alive with minimal resources: `cpus: "0.001"` and `memory: "6MB"`.
- Do not add HAProxy, Nginx, a load balancer, Unix sockets, or a second active API unless an official contest issue proves direct topology is no longer best.
- Do not weaken, delete, or rewrite `scripts/assert-compose-topology.sh` or `.github/workflows/compose-topology.yml` to permit HAProxy or any non-direct topology.

The official p99 goal is `<= 1.00ms` from the contest GitHub issue response, with:

- `false_positive_detections = 0`
- `false_negative_detections = 0`
- `http_errors = 0`

Before any PR or push that touches submission files, run:

```sh
scripts/assert-compose-topology.sh
docker compose -f docker-compose.yml config --quiet
```

If either command fails, the change is not acceptable for `submission`.
