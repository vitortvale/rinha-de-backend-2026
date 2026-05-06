#!/usr/bin/env bash
set -euo pipefail

compose_json="$(docker compose -f docker-compose.yml config --format json)"

COMPOSE_JSON="$compose_json" python3 - <<'PY'
import json
import os
import sys

config = json.loads(os.environ["COMPOSE_JSON"])
services = config.get("services", {})
expected_services = {"api1", "api2"}
actual_services = set(services)

errors = []

if actual_services != expected_services:
    errors.append(
        "docker-compose.yml must define exactly api1 and api2 for the direct topology; "
        f"found: {', '.join(sorted(actual_services)) or '<none>'}"
    )

api1 = services.get("api1", {})
api2 = services.get("api2", {})
image1 = api1.get("image")
image2 = api2.get("image")

if not image1:
    errors.append("api1 must define an image")
if not image2:
    errors.append("api2 must define an image")
if image1 and image2 and image1 != image2:
    errors.append(f"api1 and api2 must use the same image; found {image1!r} and {image2!r}")
if image1 and image1.endswith(":latest"):
    errors.append("submission must use an immutable image tag, not latest")

ports = api1.get("ports", [])
published_ports = {
    str(port.get("published"))
    for port in ports
    if isinstance(port, dict) and port.get("published") is not None
}
if "9999" not in published_ports:
    errors.append("api1 must publish port 9999")

if api1.get("environment", {}).get("TCP_PORT") != "9999":
    errors.append("api1 must set TCP_PORT=9999")

api2_command = api2.get("command")
if api2_command not in (["sleep", "infinity"], "sleep infinity"):
    errors.append("api2 must be the low-resource sleeping sidecar")

def limit(service, name):
    return (
        service.get("deploy", {})
        .get("resources", {})
        .get("limits", {})
        .get(name)
    )

if str(limit(api1, "cpus")) != "0.999":
    errors.append(f"api1 must use cpus: 0.999; found {limit(api1, 'cpus')!r}")
if str(limit(api2, "cpus")) != "0.001":
    errors.append(f"api2 must use cpus: 0.001; found {limit(api2, 'cpus')!r}")
if str(limit(api1, "memory")).lower() not in {"344mb", "344m", "360710144"}:
    errors.append(f"api1 must use memory: 344MB; found {limit(api1, 'memory')!r}")
if str(limit(api2, "memory")).lower() not in {"6mb", "6m", "6291456"}:
    errors.append(f"api2 must use memory: 6MB; found {limit(api2, 'memory')!r}")

if errors:
    print("Compose topology check failed:", file=sys.stderr)
    for error in errors:
        print(f"- {error}", file=sys.stderr)
    sys.exit(1)

print("Compose topology check passed: direct api1 + sleeping api2.")
PY
