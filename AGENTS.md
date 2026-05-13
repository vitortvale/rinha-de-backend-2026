# Agent Context

- The Docker builder image has already been built and published as `vitortvale/rinha-2026-ocaml-builder:latest`.
- That builder image uses the Oxcaml compiler switch `5.2.0+ox` with `oxcaml-compiler.5.2.0minus31`.
- Do not spend time rebuilding or changing the builder image unless the Oxcaml compiler/dependency base actually needs to change.
- The production `Dockerfile` should keep using the prebuilt builder image as its build stage.
- Keep the production implementation pure OCaml/OxCaml unless the user explicitly moves a candidate out of experiment mode.
- Rust or Go rewrites are allowed for isolated experiments in separate branches/worktrees. They may be used to evaluate runtime, load balancer, or vector-search hypotheses, but they must not replace the submission path until the user explicitly approves that direction.
- Do not add C stubs or C/AVX files to this repo.
- Use Conventional Commits for commit messages, for example `feat: ...`, `fix: ...`, `perf: ...`, `chore: ...`.
- Before opening a contest preview issue, push the repo changes and wait for the GitHub Actions image publish workflow to finish successfully.
- Contest preview issues can use the title `rt`; the body should still contain `rinha/test OCaml`.
- The publish workflow must push both `latest` and an immutable short-commit image tag. The `submission` branch should use the immutable tag with `pull_policy: always`, not mutable `latest`.
- Never use official contest test/k6 data as training data, reference data, threshold-selection data, fallback index data, or parameter-tuning data.
- Official contest test/k6 data is validation-only: use it to confirm false positives, false negatives, HTTP errors, p99, and fallback rate after changes were chosen from the three contest reference files.
- Train and tune the linear/logistic model only from the three contest reference files described in the contest README.
- Model training may be as expensive as needed if it improves runtime performance, reduces fallback rate, or lowers p99. Prefer spending CPU/time offline over adding hot-path runtime cost.
- Warm up the API before any k6/contest validation. Docker Compose readiness must not mark an API healthy until the process has loaded/touched the index and exercised the hot scoring path.

# Submission Topology Rule

- The contest submission must run exactly two active API services plus one load balancer.
- The user explicitly approved trying the custom Rust load balancer `jrblatt/so-no-forevis`. Candidate/submission experiments may replace HAProxy with that LB over the same Unix sockets. Pin the image by digest when promoting it beyond a local experiment.
- Extra services such as a dedicated vector-search/vecdb container are experiment-only unless the user explicitly accepts the contest-topology risk. They violate the current submission topology rule, so benchmark them in separate worktrees and do not merge them into `submission` without a direct user decision.
- Do not submit a direct single-active API topology, and do not turn `api2` into a sleeping sidecar.
- The `submission` branch must expose port `9999` through the load balancer only.
- `api1` and `api2` must both run the same immutable API image tag and communicate with the load balancer over the configured internal transport.
- The submission CPU distribution is fixed: `api1` gets `0.40`, `api2` gets `0.40`, and the load balancer gets `0.20`. Do not change this distribution again.
- There is intentionally no submission topology workflow or assertion script; `AGENTS.md` is the source of truth for agents.

# Performance Goal

- The only p99 that counts is the official contest GitHub issue response.
- Local smoke, benchmark, and 120s k6 runs are validation gates, not the source of truth for the goal.
- The current local p99 optimization target is `< 0.50ms`.
- Do not stop optimization work after a correct-but-slower or inconclusive run. Keep iterating on valid Docker Compose changes until a warmed 120s k6 validation through the load balancer reports `p99 <= 0.60ms` with zero false positives, zero false negatives, and zero HTTP errors, unless the user explicitly tells you to stop.
- Keep working until a local warmed 120s k6/contest validation reports p99 below `0.50ms`, then stop and let the user review the code. An official contest preview issue is not required for this local stop condition.
- Never accept false positives, false negatives, or HTTP errors as a tradeoff for latency.
- Preserve runtime correctness: `false_positive_detections = 0`, `false_negative_detections = 0`, and `http_errors = 0`.
- Stick to IVF with nprobe for the runtime search path. Do not go back to VP-tree runtime search.
- VP-tree code/artifacts may remain only as a benchmark/correctness oracle. Production runtime should stay IVF-only.
- Tuning waves should use separate worktrees and should vary `IVF_FAST_NPROBE` and `IVF_NPROBE`, with full 54,100-request correctness checks before 120s compose tests.
- Run more than one benchmark/timing pass before trusting a local improvement.
- Best valid official contest result currently observed under the required two-API-plus-LB topology: issue `#1925`, p99 `2.36ms`, zero FP/FN/HTTP errors, image `vitortvale/rinha-2026-ocaml:e8096c3`.
- Current best confirmed official result before this note: issue `#1836`, p99 `2.14ms`, zero FP/FN/HTTP errors, image `vitortvale/rinha-2026-ocaml:76e4400`, submission commit `340fe2c`, but that direct single-active topology is not valid under the contest topology rule.
- Rejected official result: issue `#1772` changed HAProxy to TCP mode with image `vitortvale/rinha-2026-ocaml:fc33de1` and submission commit `109eedf`; official p99 regressed to `3.66ms` with zero FP/FN/HTTP errors. Do not return to HAProxy TCP mode as the main path.
- Rejected official result: issue `#1775` used `IVF_FAST_NPROBE=3`, `IVF_NPROBE=24`, HAProxy HTTP mode, image `vitortvale/rinha-2026-ocaml:fc9879a`, submission commit `70a78f4`; official p99 was `2.99ms` with zero FP/FN/HTTP errors. Local exact pull-test p99 was `0.56ms`, so nprobe tuning is not solving the official tail.
- Rejected official result: issue `#1780` used tuned HAProxy HTTP mode (`nbthread 1`, `maxconn 32000`, `tune.bufsize 8192`, no backend `check`) with image `vitortvale/rinha-2026-ocaml:9f426de`, submission commit `1507f67`; official p99 regressed to `3.24ms` with zero FP/FN/HTTP errors. Do not use this HAProxy tuning as the main path.
- Local-only rejected experiment: a pure OCaml/OxCaml HTTP load balancer passed smoke and 120s with zero FP/FN/HTTP errors, but p99 was `1.65ms` locally, slower than HAProxy exact pull-tests.
- Rejected official result: issue `#1784` restored known-good HAProxy HTTP config and `IVF_FAST_NPROBE=4`, added API no-flush keep-alive response path, image `vitortvale/rinha-2026-ocaml:dc81839`, submission commit `269f47a`; official p99 regressed to `3.14ms` with zero FP/FN/HTTP errors. Do not keep the no-flush path as the main candidate.
