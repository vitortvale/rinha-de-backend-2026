#!/usr/bin/env python3
import gzip
import re
import struct
import sys


SCALE = 10000.0
RECORD = re.compile(rb'\{"vector":\[([^\]]+)\],"label":"([^"]+)"\}')


def quantize(value):
    raw = int(round((value + 1.0) * SCALE))
    if raw < 0:
        return 0
    if raw > 65535:
        return 65535
    return raw


def convert(src_path, vectors_path, labels_path):
    with gzip.open(src_path, "rb") as src:
        data = src.read()

    count = 0
    with open(vectors_path, "wb") as vectors, open(labels_path, "wb") as labels:
        for match in RECORD.finditer(data):
            values = [quantize(float(part)) for part in match.group(1).split(b",")]
            if len(values) != 14:
                raise RuntimeError(f"bad vector length at row {count}: {len(values)}")
            vectors.write(struct.pack("<14H", *values))
            labels.write(b"\x01" if match.group(2) == b"fraud" else b"\x00")
            count += 1

    print(f"converted {count} references", file=sys.stderr)


if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("usage: convert_references.py references.json.gz references.u16 labels.u8", file=sys.stderr)
        sys.exit(2)
    convert(sys.argv[1], sys.argv[2], sys.argv[3])
