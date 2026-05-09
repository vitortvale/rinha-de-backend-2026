#!/usr/bin/env python3
"""
Train a small MLP with KNN-teacher knowledge distillation for the fraud gate.

Teacher: KNN computes soft fraud probabilities on training data.
Student: 14 → H → 1 MLP with ReLU hidden layer, trained to match teacher + hard labels.
Output: JSON weights + OCaml source (--gen-ocaml / --write-ocaml PATH).

Key trick: thresholds are stored in logit space so the hot decide path
never computes exp(). Only score() (bench) does the final sigmoid.
"""

import argparse
import gzip
import json
import re
import sys
from pathlib import Path

import numpy as np
from scipy.optimize import minimize
from scipy.special import expit as sigmoid
from sklearn.neighbors import KNeighborsClassifier
from sklearn.metrics import confusion_matrix

DIM = 14
ROW_RE = re.compile(rb'\{"vector":\[([^\]]+)\],"label":"(fraud|legit)"\}')
FORBIDDEN_PARTS = {"k6", "official", "test", "test-data", "tests"}


def ensure_ref_path(path: str):
    parts = {p.lower() for p in Path(path).parts}
    name = Path(path).name.lower()
    if parts & FORBIDDEN_PARTS or "test" in name:
        raise ValueError(f"refusing to use test/official data: {path}")


def load_references(path: str, limit=None):
    ensure_ref_path(path)
    with gzip.open(path, "rb") as f:
        data = f.read()
    matches = list(ROW_RE.finditer(data))
    if limit:
        matches = matches[:limit]
    n = len(matches)
    X = np.empty((n, DIM), dtype=np.float64)
    y = np.empty(n, dtype=np.float64)
    for i, m in enumerate(matches):
        X[i] = np.fromstring(m.group(1).decode(), sep=",")
        y[i] = 1.0 if m.group(2) == b"fraud" else 0.0
    return X, y


def unpack(params, H):
    W1 = params[:DIM * H].reshape(H, DIM)
    b1 = params[DIM * H:DIM * H + H]
    W2 = params[DIM * H + H:DIM * H + H + H]
    b2 = params[-1]
    return W1, b1, W2, b2


def mlp_logit(X, W1, b1, W2, b2):
    Z1 = X @ W1.T + b1
    H = np.maximum(0.0, Z1)
    return H @ W2 + b2


def linear_logit(X, w, b):
    return X @ w + b


def loss_and_grad_linear(params, X, y_hard, y_soft, alpha, l2):
    eps = 1e-12
    N = len(X)
    w, b = params[:DIM], params[-1]
    logit = X @ w + b
    p = np.clip(sigmoid(logit), eps, 1.0 - eps)

    bce = -(y_hard * np.log(p) + (1.0 - y_hard) * np.log(1.0 - p)).mean()
    pt = np.clip(y_soft, eps, 1.0 - eps)
    kl = (pt * np.log(pt / p) + (1.0 - pt) * np.log((1.0 - pt) / (1.0 - p))).mean()
    reg = l2 * np.sum(w ** 2)

    loss = (1.0 - alpha) * bce + alpha * kl + reg

    d_logit = ((1.0 - alpha) * (p - y_hard) + alpha * (p - pt)) / N
    grad_w = X.T @ d_logit + 2.0 * l2 * w
    grad_b = d_logit.sum()

    return loss, np.append(grad_w, [grad_b])


def train_linear(X, y_hard, y_soft, alpha, l2, n_restarts, max_iter, X_warm=None, y_warm=None):
    from sklearn.linear_model import LogisticRegression

    X_w = X_warm if X_warm is not None else X
    y_w = y_warm if y_warm is not None else y_hard
    print("  sklearn lbfgs warm-up...", file=sys.stderr)
    lr = LogisticRegression(C=1.0 / max(l2, 1e-9), max_iter=2000, solver="lbfgs", n_jobs=-1)
    lr.fit(X_w, y_w.astype(int))
    p0_sklearn = np.append(lr.coef_[0], lr.intercept_[0])

    rng = np.random.default_rng(0)
    best_loss, best_params = np.inf, None
    inits = [p0_sklearn] + [
        np.append(rng.standard_normal(DIM) * np.sqrt(2.0 / DIM), [0.0])
        for _ in range(n_restarts - 1)
    ]

    for i, p0 in enumerate(inits):
        result = minimize(
            loss_and_grad_linear,
            p0,
            args=(X, y_hard, y_soft, alpha, l2),
            method="L-BFGS-B",
            jac=True,
            options={"maxiter": max_iter, "ftol": 1e-14, "gtol": 1e-8},
        )
        label = "sklearn-warmstart" if i == 0 else f"random-{i}"
        print(
            f"  restart {i+1}/{n_restarts} [{label}]: loss={result.fun:.6f}"
            f" iters={result.nit} {'ok' if result.success else 'hit-limit'}",
            file=sys.stderr,
        )
        if result.fun < best_loss:
            best_loss = result.fun
            best_params = result.x

    return best_params[:DIM], float(best_params[-1])  # w, b


def loss_and_grad(params, X, y_hard, y_soft, H, alpha, l2):
    eps = 1e-12
    N = len(X)
    W1, b1, W2, b2 = unpack(params, H)

    Z1 = X @ W1.T + b1          # (N, H)
    Act = np.maximum(0.0, Z1)    # (N, H)
    logit = Act @ W2 + b2        # (N,)
    p = np.clip(sigmoid(logit), eps, 1.0 - eps)

    # Hard BCE loss
    bce = -(y_hard * np.log(p) + (1.0 - y_hard) * np.log(1.0 - p)).mean()

    # KL distillation: KL(teacher || student) — gradient simplifies to (p - p_t)
    pt = np.clip(y_soft, eps, 1.0 - eps)
    kl = (pt * np.log(pt / p) + (1.0 - pt) * np.log((1.0 - pt) / (1.0 - p))).mean()

    # L2 on weights (not biases)
    reg = l2 * (np.sum(W1 ** 2) + np.sum(W2 ** 2))

    loss = (1.0 - alpha) * bce + alpha * kl + reg

    # Gradient w.r.t. logit: both BCE and KL reduce to (p - target) / N
    d_logit = ((1.0 - alpha) * (p - y_hard) + alpha * (p - pt)) / N  # (N,)

    # Output layer
    d_W2 = Act.T @ d_logit + 2.0 * l2 * W2   # (H,)
    d_b2 = d_logit.sum()

    # Hidden layer
    d_Act = np.outer(d_logit, W2)              # (N, H)
    d_Z1 = d_Act * (Z1 > 0.0)                 # ReLU mask
    d_W1 = d_Z1.T @ X + 2.0 * l2 * W1        # (H, DIM)
    d_b1 = d_Z1.sum(axis=0)                   # (H,)

    grad = np.concatenate([d_W1.ravel(), d_b1, d_W2, [d_b2]])
    return loss, grad


def train_mlp_sklearn(X, y_hard, H, l2):
    """Fast warm-up via sklearn MLPClassifier (adam, limited epochs)."""
    from sklearn.neural_network import MLPClassifier

    print("  sklearn adam warm-up...", file=sys.stderr)
    clf = MLPClassifier(
        hidden_layer_sizes=(H,),
        activation="relu",
        solver="adam",
        alpha=l2,
        max_iter=200,
        batch_size=2048,
        random_state=0,
        tol=1e-6,
        early_stopping=False,
    )
    clf.fit(X, y_hard.astype(int))
    W1 = clf.coefs_[0].T        # (H, DIM)
    b1 = clf.intercepts_[0]     # (H,)
    W2 = clf.coefs_[1][:, 0]   # (H,)
    b2 = clf.intercepts_[1][0]
    return np.concatenate([W1.ravel(), b1, W2, [b2]])


def train_mlp(X, y_hard, y_soft, H, alpha, l2, n_restarts, max_iter, X_warm=None, y_warm=None):
    # sklearn warm-up on larger set (X_warm) if provided, else on X
    X_w = X_warm if X_warm is not None else X
    y_w = y_warm if y_warm is not None else y_hard
    p0_sklearn = train_mlp_sklearn(X_w, y_w, H, l2)
    rng = np.random.default_rng(0)
    best_loss = np.inf
    best_params = None

    # First restart from sklearn warm-start, rest from He init
    inits = [p0_sklearn] + [
        np.concatenate([
            rng.standard_normal(DIM * H) * np.sqrt(2.0 / DIM),
            np.zeros(H),
            rng.standard_normal(H) * np.sqrt(2.0 / H),
            [0.0],
        ])
        for _ in range(n_restarts - 1)
    ]

    for i, p0 in enumerate(inits):
        result = minimize(
            loss_and_grad,
            p0,
            args=(X, y_hard, y_soft, H, alpha, l2),
            method="L-BFGS-B",
            jac=True,
            options={"maxiter": max_iter, "ftol": 1e-14, "gtol": 1e-8},
        )
        label = "sklearn-warmstart" if i == 0 else f"random-{i}"
        print(
            f"  restart {i+1}/{n_restarts} [{label}]: loss={result.fun:.6f}"
            f" iters={result.nit} {'ok' if result.success else 'hit-limit'}",
            file=sys.stderr,
        )
        if result.fun < best_loss:
            best_loss = result.fun
            best_params = result.x

    return best_params


def subsample_stratified(X, y, n, rng):
    f_idx = np.where(y == 1.0)[0]
    l_idx = np.where(y == 0.0)[0]
    n_f = min(len(f_idx), n // 3)
    n_l = min(len(l_idx), n - n_f)
    idx = np.concatenate([rng.choice(f_idx, n_f, replace=False),
                          rng.choice(l_idx, n_l, replace=False)])
    rng.shuffle(idx)
    return X[idx], y[idx]


def find_logit_thresholds(logit, y_hard, margin=1e-9):
    """Return (fraud_logit_thresh, legit_logit_thresh, coverage).

    Any sample with logit > fraud_logit_thresh is Fraud (zero FP).
    Any sample with logit < legit_logit_thresh is Legit (zero FN).
    """
    fraud_logits = logit[y_hard == 1.0]
    legit_logits = logit[y_hard == 0.0]
    # Must exceed max legit logit to be called Fraud (0 FP)
    fraud_thresh = float(legit_logits.max()) + margin
    # Must be below min fraud logit to be called Legit (0 FN)
    legit_thresh = float(fraud_logits.min()) - margin
    covered = (logit > fraud_thresh) | (logit < legit_thresh)
    return fraud_thresh, legit_thresh, float(covered.mean())


def _fmt(f: float) -> str:
    """Format float constant for OCaml source."""
    s = f"{f:.17g}"
    if "." not in s and "e" not in s and "n" not in s and "i" not in s:
        s += "."
    return s


def _term(w: float, var: str) -> str:
    if w >= 0.0:
        return f"+. ({_fmt(w)} *. {var})"
    else:
        return f"-. ({_fmt(-w)} *. {var})"


def gen_ocaml(
    W1, b1, W2, b2,
    fraud_logit_thresh, legit_logit_thresh,
    ref_legit_max, ref_fraud_hour_max, ref_fraud_merchant_max,
    linear_w=None, linear_b=None,
):
    H = len(b1)
    is_linear = (linear_w is not None)
    L = []

    def emit(*lines):
        L.extend(lines)

    emit(
        "type decision =",
        "  | Fraud",
        "  | Legit",
        "  | Unknown",
        "",
        f"(* logit thresholds — sigmoid-free hot path *)",
        f"let fraud_logit_threshold = {_fmt(fraud_logit_thresh)}",
        f"let legit_logit_threshold = {_fmt(legit_logit_thresh)}",
        f"let reference_legit_amount_vs_average_max = {ref_legit_max}",
        f"let reference_fallback_fraud_hour_max = {ref_fraud_hour_max}",
        f"let reference_fallback_fraud_merchant_avg_max = {ref_fraud_merchant_max}",
        "",
        "let[@inline always] feature query i =",
        "  (Float.of_int query.(i) *. 0.0001) -. 1.0",
        ";;",
        "",
        "let[@zero_alloc] [@inline always] reference_rule_decide query =",
        "  if query.(2) <= reference_legit_amount_vs_average_max then Legit else Unknown",
        ";;",
        "",
        "let[@zero_alloc] [@inline always] reference_fallback_rule_decide query =",
        "  if query.(3) <= reference_fallback_fraud_hour_max",
        "     || query.(13) <= reference_fallback_fraud_merchant_avg_max",
        "  then Fraud",
        "  else Unknown",
        ";;",
        "",
    )

    x_args = " ".join(f"x{i}" for i in range(DIM))
    fn_name = "model_logit"

    if is_linear:
        emit(f"let[@inline always] {fn_name} {x_args} =")
        terms = [_fmt(float(linear_b))]
        for i in range(DIM):
            terms.append(_term(float(linear_w[i]), f"x{i}"))
        emit(f"  (")
        emit(f"    {terms[0]}")
        for t in terms[1:]:
            emit(f"    {t}")
        emit(f"  )")
        emit(";;", "")
    else:
        emit(f"let[@inline always] {fn_name} {x_args} =")
        for j in range(H):
            terms = [_fmt(float(b1[j]))]
            for i in range(DIM):
                terms.append(_term(float(W1[j, i]), f"x{i}"))
            emit(f"  let h{j} = Float.max 0. (")
            emit(f"    {terms[0]}")
            for t in terms[1:]:
                emit(f"    {t}")
            emit(f"  ) in")
        out_terms = [_fmt(float(b2))]
        for j in range(H):
            out_terms.append(_term(float(W2[j]), f"h{j}"))
        emit("  (")
        emit(f"    {out_terms[0]}")
        for t in out_terms[1:]:
            emit(f"    {t}")
        emit("  )")
        emit(";;", "")

    # score: returns sigmoid(logit) for bench/debug
    emit("let score query =")
    for i in range(DIM):
        emit(f"  let x{i} = feature query {i} in")
    emit(f"  let logit = {fn_name} {x_args} in")
    emit("  1.0 /. (1.0 +. exp (-. logit))")
    emit(";;", "")

    # decide: uses logit thresholds directly (no exp in hot path)
    emit("let[@inline always] decide query =")
    emit("  match reference_rule_decide query with")
    emit("  | Legit -> Legit")
    emit("  | Fraud -> Fraud")
    emit("  | Unknown ->")
    for i in range(DIM):
        emit(f"    let x{i} = feature query {i} in")
    emit(f"    let logit = {fn_name} {x_args} in")
    emit("    if logit > fraud_logit_threshold then Fraud")
    emit("    else if logit < legit_logit_threshold then Legit")
    emit("    else reference_fallback_rule_decide query")
    emit(";;")

    return "\n".join(L)


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--references", default="resources/references.json.gz")
    p.add_argument("--hidden", type=int, default=32, help="hidden layer size")
    p.add_argument("--alpha", type=float, default=0.5,
                   help="distillation weight (0=hard labels only, 1=teacher only)")
    p.add_argument("--l2", type=float, default=1e-4, help="L2 weight decay")
    p.add_argument("--knn-k", type=int, default=50, help="KNN teacher neighbours")
    p.add_argument("--knn-subsample", type=int, default=30_000,
                   help="stratified subsample for KNN teacher fit")
    p.add_argument("--distill-subsample", type=int, default=50_000,
                   help="subsample for scipy distillation fine-tune (smaller = faster)")
    p.add_argument("--temperature", type=float, default=1.0,
                   help="teacher temperature > 1 softens probabilities")
    p.add_argument("--n-restarts", type=int, default=3)
    p.add_argument("--max-iter", type=int, default=500)
    p.add_argument("--limit", type=int, default=None)
    p.add_argument("--subsample", type=int, default=100_000,
                   help="stratified subsample for sklearn MLP warm-up "
                        "(thresholds are always evaluated on all data)")
    p.add_argument("--holdout-mod", type=int, default=0,
                   help="hold out every Nth sample for validation (applied before subsample)")
    p.add_argument("--margin", type=float, default=1e-9,
                   help="safety gap added to logit thresholds")
    p.add_argument("--gen-ocaml", action="store_true",
                   help="print OCaml source to stdout")
    p.add_argument("--write-ocaml", default=None,
                   help="write OCaml source to PATH")
    args = p.parse_args()

    print("Loading reference data...", file=sys.stderr)
    X_all, y_all = load_references(args.references, args.limit)
    print(f"  {len(X_all)} samples — {int(y_all.sum())} fraud / {int((y_all == 0).sum())} legit",
          file=sys.stderr)

    if args.holdout_mod > 1:
        mask = np.arange(len(y_all)) % args.holdout_mod == 0
        X_full, y_full = X_all[~mask], y_all[~mask]
        X_ho, y_ho = X_all[mask], y_all[mask]
    else:
        X_full, y_full = X_all, y_all
        X_ho, y_ho = X_all[:0], y_all[:0]

    # Stratified subsample for KNN teacher + MLP training.
    # Thresholds are computed on the full dataset afterwards.
    rng_ss = np.random.default_rng(42)
    if args.subsample and len(X_full) > args.subsample:
        fraud_idx = np.where(y_full == 1.0)[0]
        legit_idx = np.where(y_full == 0.0)[0]
        n_fraud = min(len(fraud_idx), args.subsample // 3)
        n_legit = args.subsample - n_fraud
        n_legit = min(n_legit, len(legit_idx))
        ss_fraud = rng_ss.choice(fraud_idx, n_fraud, replace=False)
        ss_legit = rng_ss.choice(legit_idx, n_legit, replace=False)
        ss_idx = np.concatenate([ss_fraud, ss_legit])
        rng_ss.shuffle(ss_idx)
        X_tr, y_tr = X_full[ss_idx], y_full[ss_idx]
        print(f"  subsample {len(X_tr)} — {int(y_tr.sum())} fraud / {int((y_tr==0).sum())} legit",
              file=sys.stderr)
    else:
        X_tr, y_tr = X_full, y_full

    # KNN teacher: fit on a small stratified subsample, predict on MLP training set.
    # brute-force is faster than tree methods for moderate n and D=14.
    rng_knn = np.random.default_rng(1)
    if args.knn_subsample and len(X_tr) > args.knn_subsample:
        f_idx = np.where(y_tr == 1.0)[0]
        l_idx = np.where(y_tr == 0.0)[0]
        n_kf = min(len(f_idx), args.knn_subsample // 3)
        n_kl = min(len(l_idx), args.knn_subsample - n_kf)
        knn_idx = np.concatenate([
            rng_knn.choice(f_idx, n_kf, replace=False),
            rng_knn.choice(l_idx, n_kl, replace=False),
        ])
        X_knn, y_knn = X_tr[knn_idx], y_tr[knn_idx]
    else:
        X_knn, y_knn = X_tr, y_tr

    print(f"Fitting KNN teacher (k={args.knn_k}) on {len(X_knn)} samples (brute)...",
          file=sys.stderr)
    knn = KNeighborsClassifier(n_neighbors=args.knn_k, algorithm="brute", n_jobs=-1)
    knn.fit(X_knn, y_knn)
    p_knn = knn.predict_proba(X_tr)[:, 1]

    if args.temperature != 1.0:
        eps = 1e-12
        logit_t = np.log(np.clip(p_knn, eps, 1 - eps) / (1 - np.clip(p_knn, eps, 1 - eps)))
        p_knn = sigmoid(logit_t / args.temperature)

    print(f"  KNN teacher: fraud_prob mean={p_knn[y_tr==1].mean():.3f}"
          f"  legit_prob mean={p_knn[y_tr==0].mean():.3f}", file=sys.stderr)

    # Distillation fine-tune uses a smaller subsample for scipy speed
    rng_ds = np.random.default_rng(3)
    if args.distill_subsample and len(X_tr) > args.distill_subsample:
        X_ds, y_ds = subsample_stratified(X_tr, y_tr, args.distill_subsample, rng_ds)
        # KNN teacher labels for the distill subsample via re-prediction
        p_knn_ds = knn.predict_proba(X_ds)[:, 1]
        if args.temperature != 1.0:
            eps = 1e-12
            lt = np.log(np.clip(p_knn_ds, eps, 1-eps) / (1-np.clip(p_knn_ds, eps, 1-eps)))
            p_knn_ds = sigmoid(lt / args.temperature)
        print(f"  distill subsample {len(X_ds)} for scipy fine-tune", file=sys.stderr)
    else:
        X_ds, y_ds, p_knn_ds = X_tr, y_tr, p_knn

    is_linear = (args.hidden == 0)
    if is_linear:
        print(f"Training linear model (distill, alpha={args.alpha}, l2={args.l2})...",
              file=sys.stderr)
        lin_w, lin_b = train_linear(
            X_ds, y_ds, p_knn_ds,
            args.alpha, args.l2,
            args.n_restarts, args.max_iter,
            X_warm=X_tr, y_warm=y_tr,
        )
        W1, b1, W2, b2 = np.empty((0, DIM)), np.empty(0), np.empty(0), 0.0
    else:
        print(f"Training MLP (hidden={args.hidden}, alpha={args.alpha}, l2={args.l2})...",
              file=sys.stderr)
        best_params = train_mlp(
            X_ds, y_ds, p_knn_ds,
            args.hidden, args.alpha, args.l2,
            args.n_restarts, args.max_iter,
            X_warm=X_tr, y_warm=y_tr,
        )
        W1, b1, W2, b2 = unpack(best_params, args.hidden)
        lin_w, lin_b = None, None

    # Thresholds MUST be evaluated on all full data to guarantee 0 FP/FN at runtime
    print(f"Computing thresholds on full {len(X_full)} samples...", file=sys.stderr)
    logit_full = linear_logit(X_full, lin_w, lin_b) if is_linear else mlp_logit(X_full, W1, b1, W2, b2)
    fraud_thresh, legit_thresh, coverage = find_logit_thresholds(logit_full, y_full, args.margin)

    print(f"\nTraining results:", file=sys.stderr)
    print(f"  fraud_logit_threshold = {fraud_thresh:.8f}", file=sys.stderr)
    print(f"  legit_logit_threshold = {legit_thresh:.8f}", file=sys.stderr)
    print(f"  coverage  = {coverage:.4f}  (fallback = {1-coverage:.4f})", file=sys.stderr)
    print(f"  full data rows = {len(X_full)}", file=sys.stderr)

    # Verify zero FP/FN on full set
    direct = (logit_full > fraud_thresh) | (logit_full < legit_thresh)
    if direct.any():
        pred_direct = (logit_full[direct] > fraud_thresh).astype(int)
        true_direct = y_full[direct].astype(int)
        tn, fp, fn, tp = confusion_matrix(true_direct, pred_direct, labels=[0, 1]).ravel()
        print(f"  full direct: tp={tp} tn={tn} fp={fp} fn={fn}", file=sys.stderr)
        if fp > 0 or fn > 0:
            print("  WARNING: non-zero FP/FN on full reference set!", file=sys.stderr)

    holdout_info = None
    if len(X_ho) > 0:
        logit_ho = linear_logit(X_ho, lin_w, lin_b) if is_linear else mlp_logit(X_ho, W1, b1, W2, b2)
        direct_ho = (logit_ho > fraud_thresh) | (logit_ho < legit_thresh)
        cov_ho = float(direct_ho.mean())
        pred_ho = (logit_ho[direct_ho] > fraud_thresh).astype(int)
        true_ho = y_ho[direct_ho].astype(int)
        tn, fp, fn, tp = confusion_matrix(true_ho, pred_ho, labels=[0, 1]).ravel()
        print(f"\nHoldout ({len(X_ho)} samples):", file=sys.stderr)
        print(f"  coverage={cov_ho:.4f}  tp={tp} tn={tn} fp={fp} fn={fn}", file=sys.stderr)
        holdout_info = {"rows": len(X_ho), "coverage": cov_ho, "fp": int(fp), "fn": int(fn)}

    # Reference rule thresholds (kept from existing OCaml analysis)
    ref_legit_max = 10803
    ref_fraud_hour_max = 12174
    ref_fraud_merchant_max = 10049

    result = {
        "hidden_size": args.hidden,
        "alpha": args.alpha,
        "l2": args.l2,
        "knn_k": args.knn_k,
        "temperature": args.temperature,
        "subsample_rows": len(X_tr),
        "full_rows": len(X_full),
        "fraud_logit_threshold": fraud_thresh,
        "legit_logit_threshold": legit_thresh,
        "coverage": coverage,
        "fallback_rate": 1.0 - coverage,
        "holdout": holdout_info,
        "W1": W1.tolist(),
        "b1": b1.tolist(),
        "W2": W2.tolist(),
        "b2": float(b2),
        **({"linear_w": lin_w.tolist(), "linear_b": lin_b} if is_linear else {}),
    }
    print(json.dumps(result, indent=2))

    if args.gen_ocaml or args.write_ocaml:
        ocaml_src = gen_ocaml(
            W1, b1, W2, b2,
            fraud_thresh, legit_thresh,
            ref_legit_max, ref_fraud_hour_max, ref_fraud_merchant_max,
            linear_w=lin_w, linear_b=lin_b,
        )
        if args.write_ocaml:
            with open(args.write_ocaml, "w") as f:
                f.write(ocaml_src + "\n")
            print(f"Wrote OCaml to {args.write_ocaml}", file=sys.stderr)
        if args.gen_ocaml:
            print(ocaml_src)


if __name__ == "__main__":
    main()
