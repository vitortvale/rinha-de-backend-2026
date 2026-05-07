#!/usr/bin/env python3
import argparse
import gzip
import json
from pathlib import Path
import re

import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import confusion_matrix


DIM = 14
ROW_RE = re.compile(rb'\{"vector":\[([^\]]+)\],"label":"(fraud|legit)"\}')
FORBIDDEN_REFERENCE_PATH_PARTS = {"k6", "official", "test", "test-data", "tests"}


def ensure_training_reference_path(path: str):
    parts = {part.lower() for part in Path(path).parts}
    name = Path(path).name.lower()
    if parts & FORBIDDEN_REFERENCE_PATH_PARTS or "test" in name:
        raise ValueError(
            "refusing to train on test/k6/official data; use only contest reference files"
        )


def load_references(path: str, limit: int | None):
    ensure_training_reference_path(path)
    with gzip.open(path, "rb") as f:
        data = f.read()

    matches = ROW_RE.finditer(data)
    rows = data.count(b'{"vector":[') if limit is None else limit
    x = np.empty((rows, DIM), dtype=np.float32)
    y = np.empty(rows, dtype=np.int8)

    count = 0
    for m in matches:
        if limit is not None and count >= limit:
            break
        x[count] = np.fromstring(m.group(1).decode("ascii"), sep=",", dtype=np.float32)
        y[count] = 1 if m.group(2) == b"fraud" else 0
        count += 1

    return x[:count], y[:count]


def safe_thresholds(z: np.ndarray, y: np.ndarray, margin: float):
    fraud = z[y == 1]
    legit = z[y == 0]
    low = float(fraud.min() - margin)
    high = float(legit.max() + margin)
    coverage = float(((z < low) | (z > high)).mean())
    return low, high, coverage


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--references", default="resources/references.json.gz")
    parser.add_argument("--limit", type=int, default=None)
    parser.add_argument("--c", type=float, default=1.0)
    parser.add_argument("--margin", type=float, default=1e-6)
    args = parser.parse_args()

    x, y = load_references(args.references, args.limit)
    model = LogisticRegression(
        C=args.c,
        class_weight="balanced",
        fit_intercept=True,
        max_iter=1000,
        n_jobs=-1,
        solver="lbfgs",
        tol=1e-5,
    )
    model.fit(x, y)

    weights = model.coef_[0].astype(float)
    bias = float(model.intercept_[0])
    z = x @ weights + bias
    low, high, coverage = safe_thresholds(z, y, args.margin)
    pred = (z >= 0.0).astype(np.int8)
    tn, fp, fn, tp = confusion_matrix(y, pred, labels=[0, 1]).ravel()

    result = {
        "rows": int(len(y)),
        "fraud_rows": int(y.sum()),
        "legit_rows": int(len(y) - y.sum()),
        "weights": weights.tolist(),
        "bias": bias,
        "threshold_low": low,
        "threshold_high": high,
        "safe_training_coverage": coverage,
        "decision_at_zero": {"tn": int(tn), "fp": int(fp), "fn": int(fn), "tp": int(tp)},
    }
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
