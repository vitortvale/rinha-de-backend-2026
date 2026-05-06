# Agent Instructions

This repository has a hard submission rule:

- The `submission` branch Docker Compose topology must always contain exactly two API services and one HAProxy load balancer.
- The only services allowed in `docker-compose.yml` on `submission` are `api1`, `api2`, and `haproxy`.
- `api1` and `api2` must both run the same API image.
- `api1` and `api2` must communicate through Unix sockets using `SOCKET_PATH=/sockets/api1.sock` and `SOCKET_PATH=/sockets/api2.sock`.
- Only `haproxy` may publish host port `9999`.
- Do not replace HAProxy with a direct `api1` port binding.
- Do not turn `api2` into a sleeping sidecar.
- Do not weaken, delete, or rewrite `scripts/assert-compose-topology.sh` or `.github/workflows/compose-topology.yml` to permit a direct topology.

Before any PR or push that touches submission files, run:

```sh
scripts/assert-compose-topology.sh
docker compose -f docker-compose.yml config --quiet
```

If either command fails, the change is not acceptable for `submission`.
