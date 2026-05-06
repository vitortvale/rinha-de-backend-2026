# Agent Instructions

This repository has a hard submission rule:

- The `submission` branch Docker Compose topology must always contain exactly two active API services and one load balancer.
- The only services allowed in `docker-compose.yml` on `submission` are `api1`, `api2`, and `haproxy`.
- `api1` and `api2` must both run the same immutable API image tag.
- `api1` and `api2` must communicate through Unix sockets using `SOCKET_PATH=/sockets/api1.sock` and `SOCKET_PATH=/sockets/api2.sock`.
- Only `haproxy` may publish host port `9999`.
- Do not replace HAProxy with a direct `api1` port binding.
- Do not turn `api2` into a sleeping sidecar.
- There is intentionally no topology workflow or assertion script on this branch; this `AGENTS.md` file is the source of truth for agents.
- Do not add workflow or script automation that permits a direct topology.

The official p99 goal is `<= 1.00ms` from the contest GitHub issue response, with:

- `false_positive_detections = 0`
- `false_negative_detections = 0`
- `http_errors = 0`

Before any PR or push that touches submission files, run:

```sh
docker compose -f docker-compose.yml config --quiet
```

If the compose config command fails, the change is not acceptable for `submission`.
