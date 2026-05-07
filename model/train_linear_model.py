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
    legit_coverage = float((legit < low).mean())
    fraud_coverage = float((fraud > high).mean())
    return low, high, coverage, legit_coverage, fraud_coverage


def feature_names(kind: str):
    names = [f"x{i}" for i in range(DIM)]
    if kind == "raw":
        return names
    if kind == "squares":
        return names + [f"x{i}^2" for i in range(DIM)]
    if kind == "quadratic":
        quadratic = [f"x{i}^2" for i in range(DIM)]
        quadratic.extend(f"x{i}*x{j}" for i in range(DIM) for j in range(i + 1, DIM))
        return names + quadratic
    raise ValueError(f"unknown feature map: {kind}")


def transform_features(x: np.ndarray, kind: str):
    if kind == "raw":
        return x

    parts = [x]
    if kind in {"squares", "quadratic"}:
        parts.append(x * x)

    if kind == "quadratic":
        interactions = []
        for i in range(DIM):
            xi = x[:, i : i + 1]
            for j in range(i + 1, DIM):
                interactions.append(xi * x[:, j : j + 1])
        parts.append(np.concatenate(interactions, axis=1))

    return np.concatenate(parts, axis=1).astype(np.float32, copy=False)


def split_reference_holdout(x: np.ndarray, y: np.ndarray, holdout_mod: int):
    if holdout_mod <= 1:
        return x, y, x[:0], y[:0]

    row_ids = np.arange(len(y))
    holdout = row_ids % holdout_mod == 0
    train = ~holdout
    return x[train], y[train], x[holdout], y[holdout]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--references", default="resources/references.json.gz")
    parser.add_argument("--limit", type=int, default=None)
    parser.add_argument("--c", type=float, default=1.0)
    parser.add_argument("--margin", type=float, default=1e-6)
    parser.add_argument("--legit-weight", type=float, default=None)
    parser.add_argument("--fraud-weight", type=float, default=None)
    parser.add_argument(
        "--features", choices=["raw", "squares", "quadratic"], default="raw"
    )
    parser.add_argument(
        "--holdout-mod",
        type=int,
        default=0,
        help="reference-only validation: every Nth row is held out before training",
    )
    args = parser.parse_args()

    x, y = load_references(args.references, args.limit)
    train_x, train_y, holdout_x, holdout_y = split_reference_holdout(
        x, y, args.holdout_mod
    )
    train_features = transform_features(train_x, args.features)
    class_weight = "balanced"
    if args.legit_weight is not None or args.fraud_weight is not None:
        class_weight = {
            0: 1.0 if args.legit_weight is None else args.legit_weight,
            1: 1.0 if args.fraud_weight is None else args.fraud_weight,
        }

    model = LogisticRegression(
        C=args.c,
        class_weight=class_weight,
        fit_intercept=True,
        max_iter=1000,
        n_jobs=-1,
        solver="lbfgs",
        tol=1e-5,
    )
    model.fit(train_features, train_y)

    weights = model.coef_[0].astype(float)
    bias = float(model.intercept_[0])
    train_z = train_features @ weights + bias
    low, high, coverage, legit_coverage, fraud_coverage = safe_thresholds(
        train_z, train_y, args.margin
    )
    train_pred = (train_z >= 0.0).astype(np.int8)
    tn, fp, fn, tp = confusion_matrix(train_y, train_pred, labels=[0, 1]).ravel()

    holdout_result = None
    if len(holdout_y) > 0:
        holdout_features = transform_features(holdout_x, args.features)
        holdout_z = holdout_features @ weights + bias
        direct = (holdout_z < low) | (holdout_z > high)
        holdout_pred = holdout_z > high
        direct_y = holdout_y[direct]
        direct_pred = holdout_pred[direct]
        if len(direct_y) == 0:
            h_tn = h_fp = h_fn = h_tp = 0
        else:
            h_tn, h_fp, h_fn, h_tp = confusion_matrix(
                direct_y, direct_pred, labels=[0, 1]
            ).ravel()
        holdout_result = {
            "rows": int(len(holdout_y)),
            "safe_coverage": float(direct.mean()),
            "direct_false_positives": int(h_fp),
            "direct_false_negatives": int(h_fn),
            "direct_true_positives": int(h_tp),
            "direct_true_negatives": int(h_tn),
        }

    result = {
        "rows": int(len(y)),
        "train_rows": int(len(train_y)),
        "fraud_rows": int(y.sum()),
        "legit_rows": int(len(y) - y.sum()),
        "features": args.features,
        "feature_count": int(len(weights)),
        "feature_names": feature_names(args.features),
        "weights": weights.tolist(),
        "bias": bias,
        "threshold_low": low,
        "threshold_high": high,
        "safe_training_coverage": coverage,
        "safe_training_legit_coverage": legit_coverage,
        "safe_training_fraud_coverage": fraud_coverage,
        "holdout": holdout_result,
        "decision_at_zero": {"tn": int(tn), "fp": int(fp), "fn": int(fn), "tp": int(tp)},
    }
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
