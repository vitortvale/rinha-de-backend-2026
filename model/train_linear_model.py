#!/usr/bin/env python3
import argparse
import gzip
import json
from pathlib import Path
import re

import numpy as np
from sklearn.linear_model import LogisticRegression, LogisticRegressionCV
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import GridSearchCV


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


def safe_coverage_estimator_score(estimator, x: np.ndarray, y: np.ndarray):
    z = estimator.decision_function(x)
    _low, _high, coverage, _legit_coverage, _fraud_coverage = safe_thresholds(
        z, y, 0.0
    )
    return coverage


def parse_grid_param_grid(value: str | None):
    if value is None:
        return {
            "C": [0.003, 0.01, 0.03, 0.1, 0.3, 1.0, 3.0, 10.0],
            "class_weight": [None, "balanced"],
            "fit_intercept": [True],
            "solver": ["lbfgs"],
        }

    grid = json.loads(value)
    if not isinstance(grid, dict):
        raise ValueError("--grid-param-grid must be a JSON object")
    return grid


def logit(p: float):
    return float(np.log(p / (1.0 - p)))


def sigmoid(z: float):
    return float(1.0 / (1.0 + np.exp(-z)))


def fmt_ocaml_float(value: float):
    text = f"{value:.17g}"
    if "." not in text and "e" not in text and "n" not in text and "i" not in text:
        text += "."
    return text


def ocaml_term(weight: float, name: str):
    if weight >= 0.0:
        return f"+. ({fmt_ocaml_float(weight)} *. {name})"
    return f"-. ({fmt_ocaml_float(-weight)} *. {name})"


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


def gen_ocaml_linear(
    weights: np.ndarray,
    bias: float,
    threshold_low: float,
    threshold_high: float,
    fraud_probability_threshold: float,
    features: str,
):
    if features == "raw" and len(weights) != DIM:
        raise ValueError("raw model must have 14 weights")
    if features == "squares" and len(weights) != DIM * 2:
        raise ValueError("squares model must have 28 weights")
    if features == "quadratic":
        expected = DIM + DIM + (DIM * (DIM - 1) // 2)
        if len(weights) != expected:
            raise ValueError(f"quadratic model must have {expected} weights")
    elif features not in {"raw", "squares"}:
        raise ValueError("OCaml emitter supports raw, squares, and quadratic models")

    fraud_decision_logit = logit(fraud_probability_threshold)
    legit_probability_threshold = sigmoid(threshold_low)
    fraud_gate_probability_threshold = sigmoid(threshold_high)
    lines = [
        "type decision = int",
        "",
        "(* The API fraud baseline is 0.6.  Direct gates are conservative;",
        "   uncertain rows fall back to IVF.  The hot path compares logits only. *)",
        "let unknown = -1",
        f"let fraud_probability_threshold = {fmt_ocaml_float(fraud_probability_threshold)}",
        f"let fraud_decision_logit = {fmt_ocaml_float(fraud_decision_logit)}",
        f"let legit_probability_threshold = {fmt_ocaml_float(legit_probability_threshold)}",
        f"let fraud_gate_probability_threshold = {fmt_ocaml_float(fraud_gate_probability_threshold)}",
        f"let fraud_logit_threshold = {fmt_ocaml_float(threshold_high)}",
        f"let legit_logit_threshold = {fmt_ocaml_float(threshold_low)}",
        f"let p01_logit = {fmt_ocaml_float(logit(0.1))}",
        f"let p03_logit = {fmt_ocaml_float(logit(0.3))}",
        f"let p07_logit = {fmt_ocaml_float(logit(0.7))}",
        f"let p09_logit = {fmt_ocaml_float(logit(0.9))}",
        "let reference_legit_amount_vs_average_max = 10803",
        "let reference_fallback_fraud_hour_max = 12174",
        "let reference_fallback_fraud_merchant_avg_max = 10049",
        "",
        "let[@inline always] feature query i =",
        "  (Float.of_int query.(i) *. 0.0001) -. 1.0",
        ";;",
        "",
        "let[@zero_alloc] [@inline always] reference_rule_decide query =",
        "  if query.(2) <= reference_legit_amount_vs_average_max",
        "  then 2",
        "  else unknown",
        ";;",
        "",
        "let[@zero_alloc] [@inline always] reference_fallback_rule_decide query =",
        "  if query.(3) <= reference_fallback_fraud_hour_max",
        "     || query.(13) <= reference_fallback_fraud_merchant_avg_max",
        "  then 3",
        "  else unknown",
        ";;",
        "",
        "let[@inline always] legit_bucket logit =",
        "  if logit < p01_logit then 0",
        "  else if logit < p03_logit then 1",
        "  else 2",
        ";;",
        "",
        "let[@inline always] fraud_bucket logit =",
        "  if logit >= p09_logit then 5",
        "  else if logit >= p07_logit then 4",
        "  else 3",
        ";;",
        "",
        "let[@inline always] probability_bucket logit =",
        "  if logit < fraud_decision_logit",
        "  then legit_bucket logit",
        "  else fraud_bucket logit",
        ";;",
        "",
    ]

    x_args = " ".join(f"x{i}" for i in range(DIM))
    lines.append(f"let[@inline always] model_logit {x_args} =")
    lines.append("  (")
    lines.append(f"    {fmt_ocaml_float(bias)}")
    for i, weight in enumerate(weights):
        if i < DIM:
            name = f"x{i}"
        elif i < DIM * 2:
            j = i - DIM
            name = f"(x{j} *. x{j})"
        else:
            offset = i - (DIM * 2)
            count = 0
            name = None
            for j in range(DIM):
                for k in range(j + 1, DIM):
                    if count == offset:
                        name = f"(x{j} *. x{k})"
                        break
                    count += 1
                if name is not None:
                    break
            if name is None:
                raise AssertionError(f"missing quadratic term for weight {i}")
        lines.append(f"    {ocaml_term(float(weight), name)}")
    lines.append("  )")
    lines.append(";;")
    lines.append("")

    lines.append("let score query =")
    for i in range(DIM):
        lines.append(f"  let x{i} = feature query {i} in")
    lines.append(f"  let logit = model_logit {x_args} in")
    lines.append("  1.0 /. (1.0 +. exp (-. logit))")
    lines.append(";;")
    lines.append("")

    lines.append("let[@inline always] decide_probability_bucket query =")
    for i in range(DIM):
        lines.append(f"  let x{i} = feature query {i} in")
    lines.append(f"  let logit = model_logit {x_args} in")
    lines.append("  probability_bucket logit")
    lines.append(";;")
    lines.append("")

    lines.append("let[@inline always] decide query =")
    lines.append("  let reference_decision = reference_rule_decide query in")
    lines.append("  if reference_decision >= 0")
    lines.append("  then reference_decision")
    lines.append("  else")
    for i in range(DIM):
        lines.append(f"    let x{i} = feature query {i} in")
    lines.append(f"    let logit = model_logit {x_args} in")
    lines.append("    if logit >= fraud_logit_threshold then probability_bucket logit")
    lines.append("    else if logit < legit_logit_threshold then probability_bucket logit")
    lines.append("    else reference_fallback_rule_decide query")
    lines.append(";;")

    return "\n".join(lines) + "\n"


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
    parser.add_argument(
        "--cv",
        action="store_true",
        help="use LogisticRegressionCV to choose C from the reference data",
    )
    parser.add_argument(
        "--grid-search",
        action="store_true",
        help="use GridSearchCV to choose LogisticRegression hyperparameters",
    )
    parser.add_argument(
        "--grid-param-grid",
        default=None,
        help=(
            "JSON GridSearchCV param_grid. Defaults to C x class_weight for "
            "lbfgs logistic regression."
        ),
    )
    parser.add_argument(
        "--grid-scoring",
        choices=["safe_coverage", "f1", "balanced_accuracy", "roc_auc"],
        default="safe_coverage",
        help="GridSearchCV scoring metric",
    )
    parser.add_argument(
        "--grid-jobs",
        type=int,
        default=1,
        help="parallel jobs for GridSearchCV; keep low for full 3M runs",
    )
    parser.add_argument(
        "--grid-verbose",
        type=int,
        default=0,
        help="GridSearchCV verbosity",
    )
    parser.add_argument(
        "--cv-folds",
        type=int,
        default=3,
        help="number of folds for LogisticRegressionCV",
    )
    parser.add_argument(
        "--cv-scoring",
        default="f1",
        help="scikit-learn scoring metric for LogisticRegressionCV",
    )
    parser.add_argument(
        "--cs",
        default="0.01,0.03,0.1,0.3,1.0,3.0,10.0",
        help="comma-separated C candidates for LogisticRegressionCV",
    )
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
    parser.add_argument(
        "--fraud-probability-threshold",
        type=float,
        default=0.6,
        help="probability threshold used by the emitted fraud gate",
    )
    parser.add_argument(
        "--output-ocaml",
        default=None,
        help="write src/linear_model.ml-compatible OCaml for raw models",
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

    if args.cv and args.grid_search:
        raise ValueError("--cv and --grid-search are mutually exclusive")

    if args.grid_search:
        param_grid = parse_grid_param_grid(args.grid_param_grid)
        scoring = (
            safe_coverage_estimator_score
            if args.grid_scoring == "safe_coverage"
            else args.grid_scoring
        )
        search = GridSearchCV(
            LogisticRegression(max_iter=1000, tol=1e-5),
            param_grid=param_grid,
            cv=args.cv_folds,
            error_score="raise",
            n_jobs=args.grid_jobs,
            refit=True,
            return_train_score=True,
            scoring=scoring,
            verbose=args.grid_verbose,
        )
        search.fit(train_features, train_y)
        model = search.best_estimator_
        grid_result = {
            "best_params": search.best_params_,
            "best_score": float(search.best_score_),
            "scoring": args.grid_scoring,
            "cv_results": [
                {
                    "params": params,
                    "mean_test_score": float(search.cv_results_["mean_test_score"][i]),
                    "std_test_score": float(search.cv_results_["std_test_score"][i]),
                    "rank_test_score": int(search.cv_results_["rank_test_score"][i]),
                    "mean_train_score": float(
                        search.cv_results_["mean_train_score"][i]
                    ),
                }
                for i, params in enumerate(search.cv_results_["params"])
            ],
        }
    elif args.cv:
        cs = [float(value) for value in args.cs.split(",") if value.strip()]
        model = LogisticRegressionCV(
            Cs=cs,
            class_weight=class_weight,
            cv=args.cv_folds,
            fit_intercept=True,
            max_iter=1000,
            n_jobs=-1,
            scoring=args.cv_scoring,
            solver="lbfgs",
            tol=1e-5,
        )
        grid_result = None
    else:
        model = LogisticRegression(
            C=args.c,
            class_weight=class_weight,
            fit_intercept=True,
            max_iter=1000,
            n_jobs=-1,
            solver="lbfgs",
            tol=1e-5,
        )
        grid_result = None
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
        "cv": args.cv,
        "grid_search": args.grid_search,
        "selected_c": float(model.C_[0]) if args.cv else float(model.C),
        "cv_scoring": args.cv_scoring if args.cv else None,
        "grid": grid_result,
        "feature_count": int(len(weights)),
        "feature_names": feature_names(args.features),
        "weights": weights.tolist(),
        "bias": bias,
        "fraud_probability_threshold": args.fraud_probability_threshold,
        "fraud_decision_logit": logit(args.fraud_probability_threshold),
        "legit_probability_threshold": sigmoid(low),
        "fraud_gate_probability_threshold": sigmoid(high),
        "threshold_low": low,
        "threshold_high": high,
        "bucket_probability_thresholds": [0.1, 0.3, 0.6, 0.7, 0.9],
        "safe_training_coverage": coverage,
        "safe_training_legit_coverage": legit_coverage,
        "safe_training_fraud_coverage": fraud_coverage,
        "holdout": holdout_result,
        "decision_at_zero": {"tn": int(tn), "fp": int(fp), "fn": int(fn), "tp": int(tp)},
    }
    if args.output_ocaml is not None:
        Path(args.output_ocaml).write_text(
            gen_ocaml_linear(
                weights,
                bias,
                low,
                high,
                args.fraud_probability_threshold,
                args.features,
            )
        )
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
