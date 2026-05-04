DOCKER ?= sudo docker
OCAMLFORMAT_VERSION ?= 0.26.2
OCAMLFORMAT_IMAGE ?= ocaml/opam:debian-12-ocaml-5.2
ML_FILES := $(shell find src -type f \( -name '*.ml' -o -name '*.mli' \) | sort)

.PHONY: dev build fmt fmt-check

dev:
	$(DOCKER) compose up

build:
	$(DOCKER) compose up --build

fmt:
	@if command -v ocamlformat >/dev/null 2>&1; then \
		ocamlformat --enable-outside-detected-project -i $(ML_FILES); \
	else \
		$(DOCKER) run --rm -v "$$(pwd):/src:Z" -w /src $(OCAMLFORMAT_IMAGE) sh -lc \
			'opam update -y >/dev/null && opam install -y ocamlformat.$(OCAMLFORMAT_VERSION) >/dev/null && ocamlformat --enable-outside-detected-project -i $(ML_FILES)'; \
	fi

fmt-check:
	@if command -v ocamlformat >/dev/null 2>&1; then \
		ocamlformat --enable-outside-detected-project --check $(ML_FILES); \
	else \
		$(DOCKER) run --rm -v "$$(pwd):/src:Z" -w /src $(OCAMLFORMAT_IMAGE) sh -lc \
			'opam update -y >/dev/null && opam install -y ocamlformat.$(OCAMLFORMAT_VERSION) >/dev/null && ocamlformat --enable-outside-detected-project --check $(ML_FILES)'; \
	fi
