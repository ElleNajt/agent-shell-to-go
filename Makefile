DEPS_DIR      := $(CURDIR)/tests/deps
MOCK_ACP_DIR  := $(DEPS_DIR)/mock-acp
DEPS_SENTINEL := $(DEPS_DIR)/.deps-initialized

.PHONY: test deps-init

test: deps-init
	bash tests/run.sh $(TEST)

deps-init: $(DEPS_SENTINEL)

$(DEPS_SENTINEL):
	@while IFS= read -r line || [ -n "$$line" ]; do \
	  set -- $$line; name=$$1; url=$$2; commit=$$3; \
	  dir=$(DEPS_DIR)/$$name; \
	  if [ ! -d "$$dir" ]; then git clone "$$url" "$$dir"; fi; \
	  git -C "$$dir" fetch --quiet origin; \
	  git -C "$$dir" checkout --quiet "$$commit"; \
	done < tests/deps.lock
	cd $(MOCK_ACP_DIR) && direnv allow && direnv exec . uv sync
	touch $@
