# rinha-2026

The API is written in OCaml and built with the OxCaml compiler. It scores the
fixed contest transaction payload with a logistic regression first. When the
linear model is not confident enough, it falls back to an IVF nearest-neighbor
index over the 3M reference vectors.

The regression training script lives in `model/train_linear_model.py`. It is set
up to train from the contest reference files only; k6/test data is just for
validation.

Runtime shape:

- HAProxy in front
- two identical OCaml API instances
- logistic regression hot path
- IVF fallback for uncertain rows
- no generic JSON parser in the request path

