#!/usr/bin/env bash
set -euo pipefail

compose_json="$(docker compose -f docker-compose.yml config --format json)"

COMPOSE_JSON="$compose_json" python3 - <<'PY'
import json
import os
import sys

config = json.loads(os.environ["COMPOSE_JSON"])
services = config.get("services", {})
expected_services = {"api1", "api2", "haproxy"}
actual_services = set(services)

errors = []

if actual_services != expected_services:
    errors.append(
        "docker-compose.yml must define exactly these services: "
        f"{', '.join(sorted(expected_services))}; found: "
        f"{', '.join(sorted(actual_services)) or '<none>'}"
    )

api_images = []
for name in ("api1", "api2"):
    service = services.get(name, {})
    image = service.get("image")
    if not image:
        errors.append(f"{name} must define an image")
    elif image.startswith("haproxy:") or image == "haproxy":
        errors.append(f"{name} must be an API image, not {image!r}")
    else:
        api_images.append(image)

    if service.get("hostname") != name:
        errors.append(f"{name} must set hostname: {name}")

if len(api_images) == 2 and api_images[0] != api_images[1]:
    errors.append(
        "api1 and api2 must use the same API image; "
        f"found {api_images[0]!r} and {api_images[1]!r}"
    )

haproxy = services.get("haproxy", {})
haproxy_image = haproxy.get("image")
if not (isinstance(haproxy_image, str) and haproxy_image.startswith("haproxy:")):
    errors.append(f"haproxy must use a haproxy image; found {haproxy_image!r}")

depends_on = haproxy.get("depends_on", {})
if isinstance(depends_on, list):
    dependency_names = set(depends_on)
elif isinstance(depends_on, dict):
    dependency_names = set(depends_on)
else:
    dependency_names = set()

if dependency_names != {"api1", "api2"}:
    errors.append(
        "haproxy must depend on exactly api1 and api2; "
        f"found: {', '.join(sorted(dependency_names)) or '<none>'}"
    )

ports = haproxy.get("ports", [])
published_ports = {
    str(port.get("published"))
    for port in ports
    if isinstance(port, dict) and port.get("published") is not None
}
if "9999" not in published_ports:
    errors.append("haproxy must publish port 9999")

if errors:
    print("Compose topology check failed:", file=sys.stderr)
    for error in errors:
        print(f"- {error}", file=sys.stderr)
    sys.exit(1)

print("Compose topology check passed: api1 + api2 + haproxy.")
PY
